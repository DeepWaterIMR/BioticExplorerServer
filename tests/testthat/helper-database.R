manifest_row <- function(year = 2020L, modified = "Wed, 01 Jan 2025 00:00:00 GMT",
                         checked = "2026-06-18 12:00:00 UTC") {
  data.frame(
    missiontype = "Synthetic survey",
    data_year = as.integer(year),
    platform = "Test vessel_TEST",
    delivery = paste0(year, "001"),
    last_modified = modified,
    last_snapshot_code = "snapshot-1",
    last_snapshot_time = "Wed, 01 Jan 2025 01:00:00 GMT",
    format_version = "v3.1",
    checked_at = checked,
    stringsAsFactors = FALSE
  )
}

parsed_year <- function(year = 2020L, value = "old") {
  mission <- data.frame(
    missionid = paste0(year, "_1"), missiontypename = "Synthetic survey",
    startyear = as.integer(year), platformname = "Test vessel",
    platform = "TEST", missionnumber = paste0(year, "001"),
    value = value, stringsAsFactors = FALSE
  )
  child <- data.frame(
    missionid = paste0(year, "_1"), startyear = as.integer(year),
    value = value, stringsAsFactors = FALSE
  )
  list(mission = mission, stnall = child, indall = child, ageall = child)
}

create_test_database <- function(path, schema_version = 1L, legacy = FALSE,
                                 manifest = manifest_row(), year = 2020L) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  parsed <- parsed_year(year)
  lapply(names(parsed), function(table) DBI::dbWriteTable(con, table, parsed[[table]]))
  DBI::dbWriteTable(con, "filesize", data.frame(dbyear = year, filesize = 100))
  DBI::dbWriteTable(con, "csindex", data.frame(cruiseseriescode = "1", name = "Test"))
  DBI::dbWriteTable(con, "gearindex", data.frame(code = "1"))
  metadata <- data.frame(
    timestart = "2026-01-01 00:00:00",
    timeend = "2026-01-01 01:00:00",
    stringsAsFactors = FALSE
  )
  if (!legacy) {
    metadata$package_version <- "0.7.3"
    metadata$database_schema_version <- schema_version
    metadata$update_mode <- "compile"
  }
  DBI::dbWriteTable(con, "metadata", metadata)
  if (!is.null(manifest)) DBI::dbWriteTable(con, "source_manifest", manifest)
  invisible(path)
}

reuse_test_references <- function(connection) {
  invisible(list(
    cruiseSeries = data.table::as.data.table(DBI::dbReadTable(connection, "csindex")),
    gearCodes = data.table::as.data.table(DBI::dbReadTable(connection, "gearindex"))
  ))
}
