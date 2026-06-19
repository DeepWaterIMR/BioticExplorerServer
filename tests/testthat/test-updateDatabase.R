test_that("metadata discovery keeps non-interactive logs concise", {
  deliveries <- data.frame(
    missiontype = rep("Survey", 25),
    data_year = rep(2020L, 25),
    platform = rep("Platform", 25),
    delivery = as.character(seq_len(25)),
    stringsAsFactors = FALSE
  )
  headers <- c(
    last_modified = "Wed, 01 Jan 2025 00:00:00 GMT",
    last_snapshot_code = "snapshot",
    last_snapshot_time = "2025-01-01T00:00:00Z",
    format_version = "3.1"
  )

  local_mocked_bindings(
    .discover_source_deliveries = function(years) deliveries,
    .api_delivery_headers = function(...) headers,
    .package = "BioticExplorerServer"
  )

  messages <- capture.output(
    manifest <- BioticExplorerServer:::.discover_source_manifest(verbose = FALSE),
    type = "message"
  )

  expect_equal(nrow(manifest), 25)
  expect_lte(length(messages), 11)
  expect_match(messages[[1]], "Checking metadata for 25 deliveries")
  expect_true(any(grepl("25/25 \\(100%\\)", messages)))
  expect_false(any(grepl("Survey / 2020", messages)))
})

test_that("verbose metadata discovery retains per-delivery detail", {
  deliveries <- data.frame(
    missiontype = "Survey",
    data_year = 2020L,
    platform = "Platform",
    delivery = c("1", "2"),
    stringsAsFactors = FALSE
  )
  headers <- c(
    last_modified = "Wed, 01 Jan 2025 00:00:00 GMT",
    last_snapshot_code = "snapshot",
    last_snapshot_time = "2025-01-01T00:00:00Z",
    format_version = "3.1"
  )

  local_mocked_bindings(
    .discover_source_deliveries = function(years) deliveries,
    .api_delivery_headers = function(...) headers,
    .package = "BioticExplorerServer"
  )

  messages <- capture.output(
    manifest <- BioticExplorerServer:::.discover_source_manifest(verbose = TRUE),
    type = "message"
  )

  expect_equal(nrow(manifest), 2)
  expect_length(messages, 2)
  expect_match(messages[[1]], "Checking metadata: Survey / 2020 / Platform / 1")
  expect_match(messages[[2]], "Checking metadata: Survey / 2020 / Platform / 2")
})

test_that("metadata discovery skips deliveries that disappear from the API", {
  deliveries <- data.frame(
    missiontype = "Survey",
    data_year = 2020L,
    platform = "Platform",
    delivery = c("available", "gone"),
    stringsAsFactors = FALSE
  )
  headers <- c(
    last_modified = "Wed, 01 Jan 2025 00:00:00 GMT",
    last_snapshot_code = "snapshot",
    last_snapshot_time = "2025-01-01T00:00:00Z",
    format_version = "3.1"
  )

  local_mocked_bindings(
    .discover_source_deliveries = function(years) deliveries,
    .api_delivery_headers = function(missiontype, year, platform, delivery) {
      if (delivery == "gone") NULL else headers
    },
    .package = "BioticExplorerServer"
  )

  expect_warning(
    manifest <- BioticExplorerServer:::.discover_source_manifest(verbose = FALSE),
    "Skipped 1 delivery"
  )
  expect_equal(nrow(manifest), 1)
  expect_identical(manifest$delivery, "available")
})

test_that("delivery metadata treats HTTP 404 as unavailable", {
  response <- structure(
    list(status_code = 404L),
    class = "httr2_response"
  )
  local_mocked_bindings(
    req_perform = function(request) response,
    .package = "httr2"
  )

  expect_null(BioticExplorerServer:::.api_delivery_headers(
    "Survey", 2020L, "Platform", "gone"
  ))
})

test_that("metadata discovery returns an empty manifest when all deliveries disappear", {
  deliveries <- data.frame(
    missiontype = "Survey", data_year = 2020L, platform = "Platform",
    delivery = "gone", stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    .discover_source_deliveries = function(years) deliveries,
    .api_delivery_headers = function(...) NULL,
    .package = "BioticExplorerServer"
  )

  expect_warning(
    manifest <- BioticExplorerServer:::.discover_source_manifest(verbose = FALSE),
    "Skipped 1 delivery"
  )
  expect_identical(manifest, BioticExplorerServer:::.empty_source_manifest())
})

test_that("metadata discovery stops checking a year after its first change", {
  deliveries <- data.frame(
    missiontype = rep("Survey", 3), data_year = rep(2020L, 3),
    platform = rep("Platform", 3), delivery = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  old_headers <- c(
    last_modified = "Wed, 01 Jan 2025 00:00:00 GMT",
    last_snapshot_code = "old", last_snapshot_time = "2025-01-01T00:00:00Z",
    format_version = "3.1"
  )
  stored <- do.call(rbind, lapply(seq_len(nrow(deliveries)), function(n) {
    BioticExplorerServer:::.manifest_row(
      deliveries[n, , drop = FALSE], old_headers, "2025-01-02 00:00:00 UTC"
    )
  }))
  new_headers <- old_headers
  new_headers[["last_modified"]] <- "Thu, 02 Jan 2025 00:00:00 GMT"
  calls <- 0L

  local_mocked_bindings(
    .discover_source_deliveries = function(years) deliveries,
    .api_delivery_headers = function(...) {
      calls <<- calls + 1L
      new_headers
    },
    .package = "BioticExplorerServer"
  )

  messages <- capture.output(
    manifest <- BioticExplorerServer:::.discover_source_manifest(
      verbose = FALSE, stored_manifest = stored
    ),
    type = "message"
  )
  expect_identical(calls, 1L)
  expect_identical(attr(manifest, "changed_years"), 2020L)
  expect_equal(nrow(manifest), 1)
  expect_identical(manifest$delivery, "")
  expect_true(any(grepl("skipped 2 remaining metadata requests", messages)))
})

test_that("a downloaded-year baseline does not trigger a repeated update", {
  parsed <- parsed_year()
  baseline <- BioticExplorerServer:::.baseline_manifest_from_parsed(
    parsed, 2020L, as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  deliveries <- baseline[c("missiontype", "data_year", "platform", "delivery")]
  headers <- c(
    last_modified = "Wed, 01 Jan 2025 00:00:00 GMT",
    last_snapshot_code = "snapshot", last_snapshot_time = "2025-01-01T00:00:00Z",
    format_version = "3.1"
  )

  local_mocked_bindings(
    .discover_source_deliveries = function(years) deliveries,
    .api_delivery_headers = function(...) headers,
    .package = "BioticExplorerServer"
  )

  manifest <- BioticExplorerServer:::.discover_source_manifest(
    verbose = FALSE, stored_manifest = baseline
  )
  expect_identical(attr(manifest, "changed_years"), integer())
  expect_equal(nrow(manifest), 1)
  expect_identical(manifest$last_snapshot_code, "snapshot")
})

test_that("an unchanged compatible database downloads nothing", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  manifest <- manifest_row()
  create_test_database(database, manifest = manifest)
  downloaded <- FALSE

  local_mocked_bindings(
    .refresh_reference_tables = reuse_test_references,
    .discover_source_manifest = function(years, verbose, stored_manifest = NULL) manifest,
    .download_year_for_update = function(...) {
      downloaded <<- TRUE
      stop("should not download")
    },
    indexDatabase = function(...) invisible(NULL),
    .package = "BioticExplorerServer"
  )

  result <- updateDatabase(dbPath = directory, verbose = FALSE)
  expect_identical(result$mode, "incremental")
  expect_length(result$years, 0)
  expect_false(downloaded)
})

test_that("reference tables are refreshed together", {
  database <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (table in c("csindex", "gearindex", "taxaindex", "codeindex")) {
    DBI::dbWriteTable(con, table, data.frame(value = "old"))
  }

  local_mocked_bindings(
    prepareCruiseSeriesList = function() data.frame(value = "new cruises"),
    prepareGearList = function() data.frame(value = "new gear"),
    prepareTaxaList = function() data.frame(value = "new taxa"),
    prepareReferenceCodes = function() data.frame(value = "new codes"),
    .package = "BioticExplorerServer"
  )

  references <- BioticExplorerServer:::.refresh_reference_tables(con)

  expect_identical(references$cruiseSeries$value, "new cruises")
  expect_identical(references$gearCodes$value, "new gear")
  expect_identical(DBI::dbReadTable(con, "taxaindex")$value, "new taxa")
  expect_identical(DBI::dbReadTable(con, "codeindex")$value, "new codes")
})

test_that("an empty reference download preserves existing tables", {
  database <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "csindex", data.frame(value = "old"))

  local_mocked_bindings(
    prepareCruiseSeriesList = function() data.frame(value = "new cruises"),
    prepareGearList = function() data.frame(value = character()),
    prepareTaxaList = function() data.frame(value = "new taxa"),
    prepareReferenceCodes = function() data.frame(value = "new codes"),
    .package = "BioticExplorerServer"
  )

  expect_error(
    BioticExplorerServer:::.refresh_reference_tables(con),
    "gearindex"
  )
  expect_identical(DBI::dbReadTable(con, "csindex")$value, "old")
})

test_that("a changed delivery atomically replaces its year", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  old_manifest <- manifest_row()
  new_manifest <- manifest_row(modified = "Thu, 02 Jan 2025 00:00:00 GMT")
  create_test_database(database, manifest = old_manifest)

  local_mocked_bindings(
    .refresh_reference_tables = reuse_test_references,
    .discover_source_manifest = function(years, verbose, stored_manifest = NULL) new_manifest,
    .download_year_for_update = function(...) {
      list(parsed = parsed_year(value = "new"), filesize = 200)
    },
    indexDatabase = function(...) invisible(NULL),
    .package = "BioticExplorerServer"
  )

  result <- updateDatabase(dbPath = directory, verbose = FALSE)
  expect_identical(result$years, 2020L)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM mission")$n, 1)
  expect_identical(DBI::dbGetQuery(con, "SELECT value FROM mission")$value, "new")
  expect_equal(DBI::dbGetQuery(con, "SELECT filesize FROM filesize")$filesize, 200)
  expect_identical(
    DBI::dbGetQuery(con, "SELECT last_modified FROM source_manifest")$last_modified,
    new_manifest$last_modified
  )
})

test_that("an early-exit update stores a post-download delivery baseline", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  create_test_database(database, manifest = manifest_row())
  discovered <- BioticExplorerServer:::.year_baseline_placeholder(
    2020L, "2026-01-01T00:00:00Z"
  )
  attr(discovered, "changed_years") <- 2020L

  local_mocked_bindings(
    .refresh_reference_tables = reuse_test_references,
    .discover_source_manifest = function(...) discovered,
    .download_year_for_update = function(...) {
      list(parsed = parsed_year(value = "new"), filesize = 200)
    },
    indexDatabase = function(...) invisible(NULL),
    .package = "BioticExplorerServer"
  )

  result <- updateDatabase(dbPath = directory, verbose = FALSE)
  expect_identical(result$years, 2020L)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  baseline <- DBI::dbReadTable(con, "source_manifest")
  expect_identical(baseline$delivery, "2020001")
  expect_true(all(is.na(baseline$last_modified)))
  expect_true(BioticExplorerServer:::.manifest_year_is_baseline(baseline))
})

test_that("a removed delivery removes its year", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  create_test_database(database)
  empty <- BioticExplorerServer:::.empty_source_manifest()

  local_mocked_bindings(
    .refresh_reference_tables = reuse_test_references,
    .discover_source_manifest = function(years, verbose, stored_manifest = NULL) empty,
    indexDatabase = function(...) invisible(NULL),
    .package = "BioticExplorerServer"
  )

  result <- updateDatabase(years = 2020, dbPath = directory, verbose = FALSE)
  expect_identical(result$years, 2020L)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM mission")$n, 0)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM filesize")$n, 0)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM source_manifest")$n, 0)
})

test_that("year replacement rolls back on schema mismatch", {
  database <- tempfile(fileext = ".duckdb")
  create_test_database(database)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  broken <- parsed_year(value = "new")
  broken$mission$unexpected_column <- "new schema"

  expect_error(
    BioticExplorerServer:::.write_database_year(
      con, 2020, broken, 200, replace = TRUE, manifest = manifest_row()
    ),
    "new columns"
  )
  expect_identical(DBI::dbGetQuery(con, "SELECT value FROM mission")$value, "old")
  expect_equal(DBI::dbGetQuery(con, "SELECT filesize FROM filesize")$filesize, 100)
})

test_that("database appends select data.table columns by name", {
  database <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "example", data.frame(first = "old", second = 1L))
  value <- data.table::data.table(second = 2L, first = "new")

  BioticExplorerServer:::.append_table_by_name(con, "example", value)

  result <- DBI::dbReadTable(con, "example")
  expect_identical(result$first, c("old", "new"))
  expect_identical(result$second, c(1L, 2L))
})

test_that("legacy compatible databases are stamped without rebuilding", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  manifest <- manifest_row(modified = "Wed, 01 Jan 2025 00:00:00 GMT")
  create_test_database(database, legacy = TRUE, manifest = NULL)
  rebuilt <- FALSE

  local_mocked_bindings(
    .refresh_reference_tables = reuse_test_references,
    .discover_source_manifest = function(years, verbose, stored_manifest = NULL) manifest,
    .rebuild_incompatible_database = function(...) {
      rebuilt <<- TRUE
      stop("should not rebuild")
    },
    indexDatabase = function(...) invisible(NULL),
    .package = "BioticExplorerServer"
  )

  result <- updateDatabase(dbPath = directory, verbose = FALSE)
  expect_false(rebuilt)
  expect_length(result$years, 0)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  metadata <- DBI::dbReadTable(con, "metadata")
  expect_equal(metadata$database_schema_version, 1)
})

test_that("schema mismatch chooses a full compatibility rebuild", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  create_test_database(database, schema_version = 0L)
  called <- FALSE

  local_mocked_bindings(
    .rebuild_incompatible_database = function(dbPath, dbName, dbIndexFile) {
      called <<- TRUE
      invisible(list(mode = "rebuild", years = NULL))
    },
    .package = "BioticExplorerServer"
  )

  result <- updateDatabase(years = 2020, dbPath = directory, verbose = FALSE)
  expect_true(called)
  expect_identical(result$mode, "rebuild")
  expect_null(result$years)
})

test_that("compatibility rebuild swaps only a validated complete database", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  index_file <- file.path(directory, "dbIndex.rda")
  create_test_database(database, schema_version = 0L)
  old_index <- "old"
  save(old_index, file = index_file)

  local_mocked_bindings(
    compileDatabase = function(dbPath, dbName, dbIndexFile, ...) {
      create_test_database(file.path(dbPath, paste0(dbName, ".duckdb")))
      index <- "new"
      save(index, file = dbIndexFile)
      invisible(NULL)
    },
    .package = "BioticExplorerServer"
  )

  result <- BioticExplorerServer:::.rebuild_incompatible_database(
    directory, "bioticexplorer", index_file
  )
  expect_identical(result$mode, "rebuild")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_identical(
    BioticExplorerServer:::.database_schema_status(con), "compatible"
  )
  loaded <- new.env(parent = emptyenv())
  load(index_file, envir = loaded)
  expect_identical(loaded$index, "new")
})

test_that("failed compatibility rebuild preserves the active database", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  index_file <- file.path(directory, "dbIndex.rda")
  create_test_database(database, schema_version = 0L)
  old_index <- "old"
  save(old_index, file = index_file)

  local_mocked_bindings(
    compileDatabase = function(...) stop("synthetic compile failure"),
    .package = "BioticExplorerServer"
  )

  expect_error(
    BioticExplorerServer:::.rebuild_incompatible_database(
      directory, "bioticexplorer", index_file
    ),
    "synthetic compile failure"
  )
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(DBI::dbReadTable(con, "metadata")$database_schema_version, 0)
  loaded <- new.env(parent = emptyenv())
  load(index_file, envir = loaded)
  expect_identical(loaded$old_index, "old")
})
