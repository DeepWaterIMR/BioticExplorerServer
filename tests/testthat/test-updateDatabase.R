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

test_that("an unchanged compatible database downloads nothing", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  manifest <- manifest_row()
  create_test_database(database, manifest = manifest)
  downloaded <- FALSE

  local_mocked_bindings(
    .discover_source_manifest = function(years, verbose) manifest,
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

test_that("a changed delivery atomically replaces its year", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  old_manifest <- manifest_row()
  new_manifest <- manifest_row(modified = "Thu, 02 Jan 2025 00:00:00 GMT")
  create_test_database(database, manifest = old_manifest)

  local_mocked_bindings(
    .discover_source_manifest = function(years, verbose) new_manifest,
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

test_that("a removed delivery removes its year", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  create_test_database(database)
  empty <- BioticExplorerServer:::.empty_source_manifest()

  local_mocked_bindings(
    .discover_source_manifest = function(years, verbose) empty,
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

test_that("legacy compatible databases are stamped without rebuilding", {
  directory <- withr::local_tempdir()
  database <- file.path(directory, "bioticexplorer.duckdb")
  manifest <- manifest_row(modified = "Wed, 01 Jan 2025 00:00:00 GMT")
  create_test_database(database, legacy = TRUE, manifest = NULL)
  rebuilt <- FALSE

  local_mocked_bindings(
    .discover_source_manifest = function(years, verbose) manifest,
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
