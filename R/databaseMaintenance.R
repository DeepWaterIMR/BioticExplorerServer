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
  DBI::dbAppendTable(connection, table, value[, target_fields, drop = FALSE])
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
