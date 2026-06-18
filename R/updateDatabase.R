.BIOTIC_API_BASE <- "https://biotic-api.hi.no/apis/nmdapi/biotic/v3"

.api_path <- function(...) {
  parts <- vapply(list(...), function(x) {
    utils::URLencode(as.character(x), reserved = TRUE)
  }, character(1))
  paste(c(.BIOTIC_API_BASE, parts), collapse = "/")
}

.api_get_xml <- function(url) {
  request <- httr2::request(url)
  request <- httr2::req_retry(request, max_tries = 4)
  request <- httr2::req_timeout(request, 60)
  response <- httr2::req_perform(request)
  httr2::resp_check_status(response)
  httr2::resp_body_xml(response)
}

.api_list_field <- function(url, field) {
  document <- .api_get_xml(url)
  nodes <- xml2::xml_find_all(
    document,
    paste0("//*[local-name()='element'][@name='", field, "']")
  )
  trimws(xml2::xml_text(nodes))
}

.api_delivery_headers <- function(missiontype, year, platform, delivery) {
  url <- paste0(
    .api_path(missiontype, year, platform, delivery, "dataset"),
    "?version=3.1"
  )
  request <- httr2::request(url)
  request <- httr2::req_method(request, "HEAD")
  request <- httr2::req_retry(request, max_tries = 4)
  request <- httr2::req_timeout(request, 60)
  response <- httr2::req_perform(request)
  httr2::resp_check_status(response)

  value <- function(name) {
    result <- httr2::resp_header(response, name)
    if (is.null(result)) NA_character_ else as.character(result)
  }
  out <- c(
    last_modified = value("last-modified"),
    last_snapshot_code = value("last-snapshot-code"),
    last_snapshot_time = value("last-snapshot-time"),
    format_version = value("format_version")
  )
  if (all(is.na(out[c("last_modified", "last_snapshot_code", "last_snapshot_time")]))) {
    stop("The API returned no change metadata for delivery ", delivery, ".")
  }
  out
}

.discover_source_manifest <- function(years = NULL, verbose = TRUE) {
  missiontypes <- .api_list_field(.BIOTIC_API_BASE, "missiontypename")
  checked_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  result <- list()
  n <- 0L

  for (missiontype in missiontypes) {
    available_years <- suppressWarnings(as.integer(
      .api_list_field(.api_path(missiontype), "year")
    ))
    available_years <- available_years[!is.na(available_years)]
    if (!is.null(years)) available_years <- intersect(available_years, years)

    for (year in available_years) {
      platforms <- .api_list_field(.api_path(missiontype, year), "platformpath")
      for (platform in platforms) {
        deliveries <- .api_list_field(
          .api_path(missiontype, year, platform), "delivery"
        )
        for (delivery in deliveries) {
          if (verbose) {
            message("Checking metadata: ", missiontype, " / ", year, " / ",
                    platform, " / ", delivery)
          }
          headers <- .api_delivery_headers(missiontype, year, platform, delivery)
          n <- n + 1L
          result[[n]] <- data.frame(
            missiontype = missiontype,
            data_year = as.integer(year),
            platform = platform,
            delivery = as.character(delivery),
            last_modified = unname(headers[["last_modified"]]),
            last_snapshot_code = unname(headers[["last_snapshot_code"]]),
            last_snapshot_time = unname(headers[["last_snapshot_time"]]),
            format_version = unname(headers[["format_version"]]),
            checked_at = checked_at,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (!length(result)) return(.empty_source_manifest())
  do.call(rbind, result)
}

.manifest_comparison_columns <- c(
  "missiontype", "data_year", "platform", "delivery", "last_modified",
  "last_snapshot_code", "last_snapshot_time", "format_version"
)

.normalise_manifest <- function(manifest) {
  if (!nrow(manifest)) return(manifest[, .manifest_comparison_columns, drop = FALSE])
  value <- manifest[, .manifest_comparison_columns, drop = FALSE]
  value[] <- lapply(value, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  value <- value[
    do.call(order, value[c("missiontype", "data_year", "platform", "delivery")]),
    , drop = FALSE
  ]
  rownames(value) <- NULL
  value
}

.manifest_year_equal <- function(old, current, year) {
  old_year <- old[old$data_year == year, , drop = FALSE]
  current_year <- current[current$data_year == year, , drop = FALSE]
  identical(.normalise_manifest(old_year), .normalise_manifest(current_year))
}

.local_delivery_inventory <- function(connection) {
  if (!.legacy_database_is_compatible(connection)) {
    return(data.frame())
  }
  mission <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT DISTINCT missiontypename, startyear, platformname, platform,",
      "missionnumber FROM mission"
    )
  )
  data.frame(
    missiontype = as.character(mission$missiontypename),
    data_year = as.integer(mission$startyear),
    platform = paste0(mission$platformname, "_", mission$platform),
    delivery = as.character(mission$missionnumber),
    stringsAsFactors = FALSE
  )
}

.manifest_keys <- function(value) {
  if (!nrow(value)) return(character())
  sort(unique(paste(value$missiontype, value$data_year, value$platform,
                    value$delivery, sep = "\r")))
}

.parse_api_time <- function(x) {
  suppressWarnings(as.POSIXct(x, tz = "UTC", tryFormats = c(
    "%a, %d %b %Y %H:%M:%S GMT", "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%d %H:%M:%S"
  )))
}

.legacy_year_is_current <- function(connection, manifest, year) {
  local <- .local_delivery_inventory(connection)
  local <- local[local$data_year == year, , drop = FALSE]
  remote <- manifest[manifest$data_year == year, , drop = FALSE]
  if (!identical(.manifest_keys(local), .manifest_keys(remote))) return(FALSE)

  metadata <- .database_metadata(connection)
  built <- .parse_api_time(metadata$timeend[[1]])
  signals <- c(.parse_api_time(remote$last_modified),
               .parse_api_time(remote$last_snapshot_time))
  signals <- signals[!is.na(signals)]
  !is.na(built) && (!length(signals) || built >= max(signals))
}

.download_year_for_update <- function(year, icesAreas = NULL,
                                      cruiseSeries = NULL, gearCodes = NULL) {
  destination <- tempfile(fileext = ".xml")
  on.exit(unlink(destination), add = TRUE)
  url <- paste0(.api_path(year, "cache"), "?version=3.1")
  status <- NULL
  for (attempt in seq_len(4)) {
    status <- suppressWarnings(try(
      utils::download.file(url, destination, quiet = TRUE), silent = TRUE
    ))
    if (!inherits(status, "try-error")) break
  }
  if (inherits(status, "try-error") || is.na(file.info(destination)$size)) {
    stop("Could not download Biotic data for year ", year, " after 4 attempts.")
  }
  list(
    parsed = bioticToDatabase(
      destination,
      missionidPrefix = year,
      icesAreas = icesAreas,
      cruiseSeries = cruiseSeries,
      gearCodes = gearCodes
    ),
    filesize = file.info(destination)$size
  )
}

.validate_rebuilt_database <- function(path, index_file) {
  if (!file.exists(path) || !file.exists(index_file)) return(FALSE)
  index_environment <- new.env(parent = emptyenv())
  index_valid <- !inherits(
    try(load(index_file, envir = index_environment), silent = TRUE),
    "try-error"
  ) && exists("index", envir = index_environment, inherits = FALSE)
  if (!index_valid) return(FALSE)
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  all(.BES_FACT_TABLES %in% DBI::dbListTables(connection)) &&
    identical(.database_schema_status(connection), "compatible")
}

.swap_rebuilt_database <- function(next_database, database, next_index, index_file) {
  suffix <- paste0(".backup-", Sys.getpid())
  database_backup <- paste0(database, suffix)
  index_backup <- paste0(index_file, suffix)
  database_had_old <- file.exists(database)
  index_had_old <- file.exists(index_file)

  if (database_had_old && !file.rename(database, database_backup)) {
    stop("Could not move the active database aside for the compatibility rebuild.")
  }
  if (index_had_old && !file.rename(index_file, index_backup)) {
    if (database_had_old) file.rename(database_backup, database)
    stop("Could not move the active database index aside.")
  }

  success <- file.rename(next_database, database)
  success <- success && file.rename(next_index, index_file)
  if (!success) {
    if (file.exists(database)) unlink(database)
    if (file.exists(index_file)) unlink(index_file)
    if (database_had_old) file.rename(database_backup, database)
    if (index_had_old) file.rename(index_backup, index_file)
    stop("Could not install the rebuilt database; the previous database was restored.")
  }

  if (file.exists(database_backup)) unlink(database_backup)
  if (file.exists(index_backup)) unlink(index_backup)
  invisible(NULL)
}

.rebuild_incompatible_database <- function(dbPath, dbName, dbIndexFile) {
  next_name <- paste0(dbName, "-next-", Sys.getpid())
  next_database <- normalizePath(
    paste0(file.path(dbPath, next_name), ".duckdb"), mustWork = FALSE
  )
  next_index <- normalizePath(
    file.path(dbPath, paste0("dbIndex-next-", Sys.getpid(), ".rda")),
    mustWork = FALSE
  )
  on.exit({
    if (file.exists(next_database)) unlink(next_database)
    if (file.exists(next_index)) unlink(next_index)
  }, add = TRUE)

  message("Database schema is incompatible. Building a complete replacement.")
  compileDatabase(dbPath = dbPath, dbName = next_name, dbIndexFile = next_index)
  if (!.validate_rebuilt_database(next_database, next_index)) {
    stop("The compatibility rebuild did not pass validation; the active database is unchanged.")
  }
  database <- normalizePath(
    paste0(file.path(dbPath, dbName), ".duckdb"), mustWork = FALSE
  )
  .swap_rebuilt_database(next_database, database, next_index, dbIndexFile)
  invisible(list(mode = "rebuild", years = NULL))
}

#' Incrementally update a BioticExplorer database
#'
#' Uses metadata-only API requests to identify years containing changed, added,
#' or removed deliveries. Only those annual XML caches are downloaded and
#' transactionally replaced. If the database schema is incompatible with this
#' package version, a complete sibling database is built with
#' \code{\link{compileDatabase}} and safely swapped into place.
#'
#' @param years Optional integer vector limiting metadata checks and incremental
#'   replacements. A schema compatibility rebuild always includes all years.
#' @inheritParams compileDatabase
#' @param verbose Logical; emit delivery metadata-check progress messages.
#' @return Invisibly returns a list describing the update mode and changed years.
#' @export
updateDatabase <- function(
  years = NULL,
  dbPath = "~/IMR_biotic_BES_database",
  dbIndexFile = file.path(dbPath, "dbIndex.rda"),
  dbName = NULL,
  verbose = TRUE
) {
  time_start <- Sys.time()
  if (is.null(dbName)) dbName <- "bioticexplorer"
  dbPath <- path.expand(dbPath)
  dbIndexFile <- path.expand(dbIndexFile)
  database <- normalizePath(
    paste0(file.path(dbPath, dbName), ".duckdb"), mustWork = FALSE
  )
  if (!file.exists(database)) {
    message("No existing database found. Running compileDatabase().")
    compileDatabase(dbPath = dbPath, dbName = dbName, dbIndexFile = dbIndexFile)
    return(invisible(list(mode = "compile", years = NULL)))
  }

  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = database)
  connection_open <- TRUE
  on.exit({
    if (connection_open) {
      try(DBI::dbDisconnect(connection, shutdown = TRUE), silent = TRUE)
    }
  }, add = TRUE)
  status <- .database_schema_status(connection)
  if (identical(status, "incompatible")) {
    DBI::dbDisconnect(connection, shutdown = TRUE)
    connection_open <- FALSE
    return(.rebuild_incompatible_database(dbPath, dbName, dbIndexFile))
  }

  if (identical(status, "legacy-compatible")) {
    old_metadata <- .database_metadata(connection)
    .write_database_metadata(
      connection, old_metadata$timestart[[1]], old_metadata$timeend[[1]],
      update_mode = "legacy-migration"
    )
  }
  .ensure_source_manifest(connection)

  requested_years <- if (is.null(years)) NULL else sort(unique(as.integer(years)))
  current_manifest <- .discover_source_manifest(requested_years, verbose = verbose)
  stored_manifest <- DBI::dbReadTable(connection, "source_manifest")
  local_years <- if ("mission" %in% DBI::dbListTables(connection)) {
    DBI::dbGetQuery(connection, "SELECT DISTINCT startyear FROM mission")$startyear
  } else integer()
  target_years <- if (is.null(requested_years)) {
    sort(unique(c(current_manifest$data_year, stored_manifest$data_year, local_years)))
  } else requested_years

  legacy_manifest <- !nrow(stored_manifest)
  changed_years <- target_years[vapply(target_years, function(year) {
    if (legacy_manifest) {
      !.legacy_year_is_current(connection, current_manifest, year)
    } else {
      !.manifest_year_equal(stored_manifest, current_manifest, year)
    }
  }, logical(1))]

  cruiseSeries <- if ("csindex" %in% DBI::dbListTables(connection)) {
    data.table::as.data.table(DBI::dbReadTable(connection, "csindex"))
  } else NULL
  gearCodes <- if ("gearindex" %in% DBI::dbListTables(connection)) {
    data.table::as.data.table(DBI::dbReadTable(connection, "gearindex"))
  } else NULL

  for (year in changed_years) {
    year_manifest <- current_manifest[current_manifest$data_year == year, , drop = FALSE]
    if (nrow(year_manifest)) {
      message("Downloading changed year: ", year)
      downloaded <- .download_year_for_update(
        year, cruiseSeries = cruiseSeries, gearCodes = gearCodes
      )
      .write_database_year(
        connection, year, downloaded$parsed, downloaded$filesize,
        replace = TRUE, manifest = year_manifest
      )
    } else {
      message("Removing year no longer present in the API: ", year)
      .write_database_year(
        connection, year, list(), replace = TRUE,
        manifest = .empty_source_manifest()
      )
    }
  }

  unchanged_years <- setdiff(target_years, changed_years)
  if (length(unchanged_years)) {
    DBI::dbWithTransaction(connection, {
      for (year in unchanged_years) {
        DBI::dbExecute(connection, "DELETE FROM source_manifest WHERE data_year = ?",
                       params = list(as.integer(year)))
        rows <- current_manifest[current_manifest$data_year == year, , drop = FALSE]
        .append_table_by_name(connection, "source_manifest", rows)
      }
    })
  }

  .write_database_metadata(connection, time_start, Sys.time(), update_mode = "incremental")
  indexDatabase(connection = connection, dbIndexFile = dbIndexFile)
  DBI::dbDisconnect(connection, shutdown = TRUE)
  connection_open <- FALSE
  message(if (length(changed_years)) {
    paste("Updated years:", paste(changed_years, collapse = ", "))
  } else "Database is already up to date.")
  invisible(list(mode = "incremental", years = changed_years))
}
