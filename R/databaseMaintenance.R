.BES_DATABASE_SCHEMA_VERSION <- 1L
.BES_FACT_TABLES <- c("mission", "stnall", "indall", "ageall")

.bes_package_version <- function() {
  as.character(utils::packageVersion("BioticExplorerServer"))
}

.database_metadata <- function(connection) {
  if (!"metadata" %in% DBI::dbListTables(connection)) {
    return(NULL)
  }
  DBI::dbReadTable(connection, "metadata")
}

.write_database_metadata <- function(connection, timestart, timeend,
                                     update_mode = "compile") {
  DBI::dbWriteTable(
    connection,
    "metadata",
    data.frame(
      timestart = as.character(timestart),
      timeend = as.character(timeend),
      package_version = .bes_package_version(),
      database_schema_version = .BES_DATABASE_SCHEMA_VERSION,
      update_mode = update_mode,
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )
}

.legacy_database_is_compatible <- function(connection) {
  required_tables <- c(.BES_FACT_TABLES, "filesize", "metadata", "csindex")
  if (!all(required_tables %in% DBI::dbListTables(connection))) {
    return(FALSE)
  }

  required_fields <- list(
    mission = c("missionid", "missiontypename", "startyear", "platformname",
                "platform", "missionnumber"),
    stnall = c("missionid", "startyear"),
    indall = c("missionid", "startyear"),
    ageall = c("missionid", "startyear"),
    filesize = c("dbyear", "filesize"),
    metadata = c("timestart", "timeend")
  )

  all(vapply(names(required_fields), function(table) {
    all(required_fields[[table]] %in% DBI::dbListFields(connection, table))
  }, logical(1)))
}

.database_schema_status <- function(connection) {
  metadata <- .database_metadata(connection)
  if (is.null(metadata) || !"database_schema_version" %in% names(metadata)) {
    return(if (.legacy_database_is_compatible(connection)) "legacy-compatible" else "incompatible")
  }

  value <- suppressWarnings(as.integer(metadata$database_schema_version[[1]]))
  if (!is.na(value) && identical(value, .BES_DATABASE_SCHEMA_VERSION) &&
      .legacy_database_is_compatible(connection)) {
    "compatible"
  } else {
    "incompatible"
  }
}

.empty_source_manifest <- function() {
  data.frame(
    missiontype = character(),
    data_year = integer(),
    platform = character(),
    delivery = character(),
    last_modified = character(),
    last_snapshot_code = character(),
    last_snapshot_time = character(),
    format_version = character(),
    checked_at = character(),
    stringsAsFactors = FALSE
  )
}

.ensure_source_manifest <- function(connection) {
  if (!"source_manifest" %in% DBI::dbListTables(connection)) {
    DBI::dbWriteTable(connection, "source_manifest", .empty_source_manifest())
  }
}

.append_table_by_name <- function(connection, table, value) {
  if (!nrow(value)) return(invisible(NULL))
  if (!table %in% DBI::dbListTables(connection)) {
    DBI::dbWriteTable(connection, table, value)
    return(invisible(NULL))
  }

  target_fields <- DBI::dbListFields(connection, table)
  missing_in_value <- setdiff(target_fields, names(value))
  for (field in missing_in_value) value[[field]] <- NA
  extra_fields <- setdiff(names(value), target_fields)
  if (length(extra_fields)) {
    stop(
      "The downloaded ", table, " table contains new columns (",
      paste(extra_fields, collapse = ", "),
      "). Increment the database schema version and rebuild the database."
    )
  }
  value <- if (data.table::is.data.table(value)) {
    value[, target_fields, with = FALSE]
  } else {
    value[, target_fields, drop = FALSE]
  }
  DBI::dbAppendTable(connection, table, value)
  invisible(NULL)
}

.write_database_year <- function(connection, year, parsed, filesize = NULL,
                                 replace = FALSE, manifest = NULL) {
  DBI::dbWithTransaction(connection, {
    if (replace) {
      for (table in intersect(.BES_FACT_TABLES, DBI::dbListTables(connection))) {
        DBI::dbExecute(
          connection,
          paste0("DELETE FROM ", DBI::dbQuoteIdentifier(connection, table),
                 " WHERE startyear = ?"),
          params = list(as.integer(year))
        )
      }
      if ("filesize" %in% DBI::dbListTables(connection)) {
        DBI::dbExecute(connection, "DELETE FROM filesize WHERE dbyear = ?",
                       params = list(as.integer(year)))
      }
    }

    for (table in names(parsed)) {
      .append_table_by_name(connection, table, parsed[[table]])
    }
    if (!is.null(filesize)) {
      .append_table_by_name(
        connection,
        "filesize",
        data.frame(dbyear = as.integer(year), filesize = as.numeric(filesize))
      )
    }

    if (!is.null(manifest)) {
      .ensure_source_manifest(connection)
      DBI::dbExecute(connection, "DELETE FROM source_manifest WHERE data_year = ?",
                     params = list(as.integer(year)))
      .append_table_by_name(connection, "source_manifest", manifest)
    }
  })
  invisible(NULL)
}

.database_has_year <- function(connection, year) {
  if (!"mission" %in% DBI::dbListTables(connection)) return(FALSE)
  query <- DBI::dbGetQuery(
    connection,
    "SELECT 1 AS found FROM mission WHERE startyear = ? LIMIT 1",
    params = list(as.integer(year))
  )
  nrow(query) > 0
}

# Future releases that alter fact-table columns or derived-data semantics must
# increment .BES_DATABASE_SCHEMA_VERSION. updateDatabase() uses this value to
# decide whether an incremental update is safe.

# Declare character columns of a data.table as UTF-8 (in place).
.mark_utf8 <- function(x) {
  if (!data.table::is.data.table(x)) return(x)
  for (col in names(x)[vapply(x, is.character, logical(1))]) {
    data.table::set(x, j = col, value = `Encoding<-`(x[[col]], "UTF-8"))
  }
  x
}

# Turn R's "<c3><a5>" byte-escape text back into UTF-8. Values that would not
# decode to valid UTF-8 are returned unchanged.
.decode_escaped_bytes <- function(x) {
  out <- vapply(x, function(s) {
    hex <- regmatches(s, gregexpr("<[0-9a-f]{2}>", s))[[1]]
    rest <- strsplit(s, "<[0-9a-f]{2}>", perl = TRUE)[[1]]
    rest <- c(rest, rep("", length(hex) + 1L - length(rest)))
    bytes <- c(charToRaw(rest[1]), unlist(lapply(seq_along(hex), function(i) {
      c(as.raw(strtoi(substr(hex[i], 2, 3), 16L)), charToRaw(rest[i + 1L]))
    })))
    rawToChar(bytes)
  }, character(1), USE.NAMES = FALSE)
  Encoding(out) <- "UTF-8"
  ok <- validUTF8(out)
  out[!ok] <- x[!ok]
  out
}

#' @title Repair byte-escaped text in a BioticExplorer database
#' @description Databases written under a non-UTF-8 locale (for example a background \code{Rscript} with \code{LANG} unset) by \code{BioticExplorerServer} <= 0.8.8 store non-ASCII characters as literal escape text, so that \code{"blåkveite"} becomes \code{"bl<c3><a5>kveite"}. This function finds such values in all text columns of the \code{mission}, \code{stnall}, \code{indall} and \code{ageall} tables, decodes them back to UTF-8 in place and re-indexes the database.
#' @param dbPath Character string giving the path to the DuckDB database file.
#' @param dbIndexFile Character string giving the path to the index file. Rebuilt after a repair. Set to \code{NULL} to skip re-indexing.
#' @param dryRun Logical. If \code{TRUE}, only reports the number of affected rows per table and column.
#' @return A data frame with one row per repaired table and column, giving the number of distinct values and rows changed. Returned invisibly unless \code{dryRun = TRUE}.
#' @author Mikko Vihtakari
#' @export

repairDatabaseEncoding <- function(dbPath = file.path(defaultDbPath(), "bioticexplorer.duckdb"),
                                   dbIndexFile = file.path(dirname(dbPath), "dbIndex.rda"),
                                   dryRun = FALSE) {
  dbPath <- path.expand(dbPath)
  if (!file.exists(dbPath)) stop("Database not found: ", dbPath)

  connection <- DBI::dbConnect(duckdb::duckdb(), dbPath, read_only = dryRun)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  pattern <- "<[0-9a-f]{2}>"
  columns <- DBI::dbGetQuery(connection, paste0(
    "SELECT table_name, column_name FROM information_schema.columns ",
    "WHERE data_type = 'VARCHAR' AND table_name IN (",
    paste0("'", .BES_FACT_TABLES, "'", collapse = ", "), ")"
  ))

  report <- do.call(rbind, lapply(seq_len(nrow(columns)), function(i) {
    tab <- DBI::dbQuoteIdentifier(connection, columns$table_name[i])
    col <- DBI::dbQuoteIdentifier(connection, columns$column_name[i])
    old <- DBI::dbGetQuery(connection, paste0(
      "SELECT DISTINCT ", col, " AS old FROM ", tab,
      " WHERE regexp_matches(", col, ", '", pattern, "')"
    ))$old
    if (!length(old)) return(NULL)
    map <- data.frame(old = old, new = .decode_escaped_bytes(old), stringsAsFactors = FALSE)
    map <- map[map$old != map$new, , drop = FALSE]
    if (!nrow(map)) return(NULL)

    if (dryRun) {
      rows <- DBI::dbGetQuery(connection, paste0(
        "SELECT count(*) AS n FROM ", tab, " WHERE regexp_matches(", col, ", '", pattern, "')"
      ))$n
    } else {
      DBI::dbWriteTable(connection, "bes_encoding_map", map, temporary = TRUE, overwrite = TRUE)
      rows <- DBI::dbWithTransaction(connection, DBI::dbExecute(connection, paste0(
        "UPDATE ", tab, " SET ", col, " = m.new FROM bes_encoding_map m WHERE ", tab, ".", col, " = m.old"
      )))
      DBI::dbRemoveTable(connection, "bes_encoding_map", temporary = TRUE)
    }
    message(columns$table_name[i], ".", columns$column_name[i], ": ", rows, " rows")
    data.frame(table = columns$table_name[i], column = columns$column_name[i],
               values = nrow(map), rows = as.numeric(rows), stringsAsFactors = FALSE)
  }))

  if (is.null(report)) {
    message("No byte-escaped text found.")
    report <- data.frame(table = character(), column = character(), values = integer(), rows = numeric())
  }

  if (dryRun) return(report)

  if (nrow(report) && !is.null(dbIndexFile)) {
    indexDatabase(connection = connection, dbIndexFile = dbIndexFile)
  }
  invisible(report)
}
