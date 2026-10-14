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

# R escapes bytes it cannot represent in a non-UTF-8 native encoding as "<xx>".
# Only bytes >= 0x80 are escaped, so ASCII text such as "<10>" is never touched.
.ESCAPED_BYTE_PATTERN <- "<[89a-f][0-9a-f]>"

# Run expr with a UTF-8 LC_CTYPE. Biotic text is UTF-8; under a C/POSIX locale R
# and its database drivers mangle non-ASCII characters.
.needs_utf8_locale <- function() {
  !isTRUE(l10n_info()[["UTF-8"]]) && !isTRUE(getOption("BioticExplorerServer.locale_checked"))
}

.with_utf8_locale <- function(expr) {
  if (!.needs_utf8_locale()) return(expr)
  old_option <- options(BioticExplorerServer.locale_checked = TRUE)
  on.exit(options(old_option), add = TRUE)
  old <- Sys.getlocale("LC_CTYPE")
  candidates <- c("C.UTF-8", "en_US.UTF-8", "en_GB.UTF-8", "nb_NO.UTF-8", "UTF-8")
  for (locale in candidates) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", locale)))) break
  }
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    warning("Could not switch to a UTF-8 locale (current LC_CTYPE: ", old, "). ",
            "Set LANG/LC_ALL to a UTF-8 locale before running BioticExplorerServer.",
            call. = FALSE)
  } else {
    message("Temporarily using LC_CTYPE=", Sys.getlocale("LC_CTYPE"),
            " (was ", old, ") so that Norwegian characters are stored correctly.")
    on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)
  }
  force(expr)
}

# Count values with byte-escape text in the columns most likely to carry
# Norwegian characters. Cheap enough to run after every update.
.count_escaped_text <- function(connection) {
  checks <- list(mission = "platformname", stnall = "commonname")
  sum(vapply(names(checks), function(table) {
    if (!table %in% DBI::dbListTables(connection) ||
        !checks[[table]] %in% DBI::dbListFields(connection, table)) return(0)
    as.numeric(DBI::dbGetQuery(connection, paste0(
      "SELECT count(*) AS n FROM ", table, " WHERE regexp_matches(",
      checks[[table]], ", '", .ESCAPED_BYTE_PATTERN, "')"
    ))$n)
  }, numeric(1)))
}

# Turn "<c3><a5>" byte-escape text back into UTF-8. Values that would not
# decode to valid UTF-8 are returned unchanged.
.decode_escaped_bytes <- function(x) {
  out <- vapply(x, function(s) {
    hex <- regmatches(s, gregexpr(.ESCAPED_BYTE_PATTERN, s))[[1]]
    rest <- strsplit(s, .ESCAPED_BYTE_PATTERN, perl = TRUE)[[1]]
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

#' @title Check and repair byte-escaped text in a BioticExplorer database
#' @description Databases compiled or updated by \code{BioticExplorerServer} <= 0.8.8 under a non-UTF-8 locale (for example a background \code{Rscript} started with \code{LANG} unset) store Norwegian characters as literal escape text, so that \code{"blåkveite"} becomes \code{"bl<c3><a5>kveite"}. This affects every text column: species, platform and mission-type names, and all comment fields. The function finds such values in all tables of the database, decodes them back to UTF-8 in place and re-indexes the database. Use \code{dryRun = TRUE} to only check. An alternative to repairing is to re-download the whole database with \code{compileDatabase(overwrite = TRUE)} using BioticExplorerServer >= 0.8.9.
#' @param dbIndexFile Character string giving the path to the index file rebuilt after a repair. \code{NULL} skips re-indexing.
#' @param dryRun Logical. If \code{TRUE}, opens the database read-only and only reports the affected tables and columns.
#' @inheritParams updateDatabase
#' @return A data frame with one row per affected table and column, giving the number of distinct values and rows. Returned invisibly unless \code{dryRun = TRUE}. A zero-row data frame means the database has no byte-escaped text.
#' @details Close all other connections to the database (R sessions, BioticExplorer) before repairing. Only escape sequences for bytes >= 0x80 that decode to valid UTF-8 are changed, so legitimate text such as \code{"<10>"} is left alone.
#' @author Mikko Vihtakari
#' @examples
#' \dontrun{
#' repairDatabaseEncoding(dryRun = TRUE) # check
#' repairDatabaseEncoding()              # repair and re-index
#' }
#' @export

repairDatabaseEncoding <- function(dbPath = defaultDbPath(),
                                   dbIndexFile = file.path(dbPath, "dbIndex.rda"),
                                   dbName = NULL,
                                   dryRun = FALSE) {
  .with_utf8_locale({
  if (is.null(dbName)) dbName <- "bioticexplorer"
  dbPath <- path.expand(dbPath)
  if (!is.null(dbIndexFile)) dbIndexFile <- path.expand(dbIndexFile)
  database <- normalizePath(paste0(file.path(dbPath, dbName), ".duckdb"), mustWork = FALSE)
  if (!file.exists(database)) stop("Database not found: ", database)

  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = database, read_only = dryRun)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  pattern <- .ESCAPED_BYTE_PATTERN
  columns <- DBI::dbGetQuery(connection, paste0(
    "SELECT c.table_name, c.column_name FROM information_schema.columns c ",
    "JOIN information_schema.tables t USING (table_schema, table_name) ",
    "WHERE c.data_type = 'VARCHAR' AND t.table_type = 'BASE TABLE' ",
    "AND c.table_schema = 'main'"
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
        "UPDATE ", tab, " SET ", col, " = m.new FROM bes_encoding_map m WHERE ",
        tab, ".", col, " = m.old"
      )))
      DBI::dbRemoveTable(connection, "bes_encoding_map", temporary = TRUE)
    }
    message(columns$table_name[i], ".", columns$column_name[i], ": ", rows, " rows")
    data.frame(table = columns$table_name[i], column = columns$column_name[i],
               values = nrow(map), rows = as.numeric(rows), stringsAsFactors = FALSE)
  }))

  if (is.null(report)) {
    message("No byte-escaped text found in ", database, ".")
    report <- data.frame(table = character(), column = character(),
                         values = integer(), rows = numeric(), stringsAsFactors = FALSE)
  }

  if (dryRun) return(report)

  if (nrow(report)) {
    remaining <- .count_escaped_text(connection)
    if (remaining > 0) warning(remaining, " byte-escaped values could not be repaired.", call. = FALSE)
    if (!is.null(dbIndexFile)) indexDatabase(connection = connection, dbIndexFile = dbIndexFile)
  }
  invisible(report)
  })
}
