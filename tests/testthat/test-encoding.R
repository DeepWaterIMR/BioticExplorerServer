test_that("byte-escaped text decodes back to UTF-8", {
  x <- c("bl<c3><a5>kveite", "s<c3><b8>lvkveite", "torsk", "<c3><a6>rfugl<c3><a5>", "bad<ff>",
         "lengde <10> cm", "ok <c3><a5> og <1f>")
  y <- .decode_escaped_bytes(x)
  expect_identical(y[1:4], c("blåkveite", "sølvkveite", "torsk", "ærfuglå"))
  expect_identical(y[5], "bad<ff>")
  expect_identical(y[6], "lengde <10> cm")
  expect_identical(y[7], "ok å og <1f>")
})

test_that("UTF-8 marked strings survive a duckdb write under any locale", {
  skip_if_not_installed("duckdb")
  dt <- data.table::data.table(x = rawToChar(as.raw(c(0x62, 0x6c, 0xc3, 0xa5))), n = 1L)
  .mark_utf8(dt)
  expect_identical(Encoding(dt$x), "UTF-8")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", dt)
  expect_identical(DBI::dbGetQuery(con, "SELECT hex(x) AS h FROM t")$h, "626CC3A5")
})

test_that("repairDatabaseEncoding() checks and repairs a database in place", {
  skip_if_not_installed("duckdb")
  dir <- withr::local_tempdir()
  con <- DBI::dbConnect(duckdb::duckdb(), file.path(dir, "bioticexplorer.duckdb"))
  DBI::dbWriteTable(con, "stnall", data.frame(
    commonname = c("bl<c3><a5>kveite", "blåkveite", "torsk", NA),
    catchcomment = c("st<c3><b8>rre enn <10> cm", NA, "ok", "x"),
    n = 1:4
  ))
  DBI::dbWriteTable(con, "mission", data.frame(platformname = "Johan Hjort"))
  DBI::dbDisconnect(con, shutdown = TRUE)

  check <- suppressMessages(repairDatabaseEncoding(dir, dryRun = TRUE))
  expect_setequal(check$column, c("commonname", "catchcomment"))
  expect_equal(sum(check$rows), 2)

  fixed <- suppressMessages(repairDatabaseEncoding(dir, dbIndexFile = NULL))
  expect_equal(nrow(fixed), 2)

  con <- DBI::dbConnect(duckdb::duckdb(), file.path(dir, "bioticexplorer.duckdb"), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  out <- DBI::dbReadTable(con, "stnall")
  expect_identical(out$commonname[1:2], c("blåkveite", "blåkveite"))
  expect_identical(out$catchcomment[1], "større enn <10> cm")
  expect_equal(.count_escaped_text(con), 0)
  expect_equal(nrow(suppressMessages(repairDatabaseEncoding(dir, dryRun = TRUE))), 0)
})

test_that("the UTF-8 locale guard does not recurse", {
  withr::local_options(BioticExplorerServer.locale_checked = TRUE)
  expect_false(.needs_utf8_locale())
  expect_identical(.with_utf8_locale(1 + 1), 2)
})
