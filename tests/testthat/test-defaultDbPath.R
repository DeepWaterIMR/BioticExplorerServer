test_that("defaultDbPath uses ~ expansion outside Windows", {
  local_mocked_bindings(.isWindows = function() FALSE)

  expect_identical(
    defaultDbPath(),
    path.expand("~/Fiskwise_data/IMR_biotic_BES_database")
  )
  expect_identical(
    defaultDbPath(folder = "custom_folder"),
    path.expand("~/Fiskwise_data/custom_folder")
  )
  expect_identical(
    defaultDbPath(root = NULL),
    path.expand("~/IMR_biotic_BES_database")
  )
  expect_identical(
    defaultDbPath(root = ""),
    path.expand("~/IMR_biotic_BES_database")
  )
})

test_that("defaultDbPath uses USERPROFILE on Windows", {
  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(USERPROFILE = "C:/Users/tester")

  expect_identical(
    defaultDbPath(),
    "C:/Users/tester/Fiskwise_data/IMR_biotic_BES_database"
  )
})

test_that("defaultDbPath falls back to ~ when USERPROFILE is unset", {
  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(USERPROFILE = "")

  expect_identical(
    defaultDbPath(),
    path.expand("~/Fiskwise_data/IMR_biotic_BES_database")
  )
})

test_that("dbPathCandidates lists current and legacy Windows locations", {
  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(USERPROFILE = "C:/Users/tester")

  candidates <- dbPathCandidates()

  expect_identical(candidates, c(
    "C:/Users/tester/Fiskwise_data/IMR_biotic_BES_database",
    path.expand("~/Fiskwise_data/IMR_biotic_BES_database"),
    "C:/Users/tester/IMR_biotic_BES_database",
    path.expand("~/IMR_biotic_BES_database")
  ))
})

test_that("dbPathCandidates lists current then legacy path outside Windows", {
  local_mocked_bindings(.isWindows = function() FALSE)

  expect_identical(dbPathCandidates(), c(
    path.expand("~/Fiskwise_data/IMR_biotic_BES_database"),
    path.expand("~/IMR_biotic_BES_database")
  ))
})

test_that("findDatabase returns NULL when no database exists", {
  local_mocked_bindings(.isWindows = function() FALSE)
  withr::local_envvar(HOME = withr::local_tempdir())

  expect_null(findDatabase())
})

make_db <- function(...) {
  dir <- file.path(...)
  dir.create(dir, recursive = TRUE)
  file.create(file.path(dir, "bioticexplorer.duckdb"))
  normalizePath(
    file.path(dir, "bioticexplorer.duckdb"), winslash = "/", mustWork = FALSE
  )
}

test_that("findDatabase prefers Fiskwise_data over the legacy home location", {
  home <- withr::local_tempdir()
  local_mocked_bindings(.isWindows = function() FALSE)
  withr::local_envvar(HOME = home)

  legacy <- make_db(home, "IMR_biotic_BES_database")
  expect_identical(findDatabase(), legacy)

  current <- make_db(home, "Fiskwise_data", "IMR_biotic_BES_database")
  expect_identical(findDatabase(), current)
})

test_that("findDatabase prefers USERPROFILE over ~ expansion on Windows", {
  home <- withr::local_tempdir()
  profile <- withr::local_tempdir()
  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(HOME = home, USERPROFILE = profile)

  make_db(home, "IMR_biotic_BES_database")
  expected <- make_db(profile, "IMR_biotic_BES_database")

  expect_identical(findDatabase(), expected)
})

test_that("findDatabase falls back to the oldest location and honours dbName", {
  home <- withr::local_tempdir()
  profile <- withr::local_tempdir()
  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(HOME = home, USERPROFILE = profile)

  expected <- make_db(home, "IMR_biotic_BES_database")

  expect_identical(findDatabase(), expected)
  expect_null(findDatabase(dbName = "does_not_exist"))
})

test_that(".existingDbPath follows a legacy install and defaults to the new folder", {
  home <- withr::local_tempdir()
  local_mocked_bindings(.isWindows = function() FALSE)
  withr::local_envvar(HOME = home)

  expect_identical(.existingDbPath(), defaultDbPath())

  legacy <- make_db(home, "IMR_biotic_BES_database")
  expect_identical(.existingDbPath(), dirname(legacy))
  expect_identical(.existingDbPath("other"), defaultDbPath())
})
