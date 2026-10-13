test_that("byte-escaped text decodes back to UTF-8", {
  x <- c("bl<c3><a5>kveite", "s<c3><b8>lvkveite", "torsk", "<c3><a6>rfugl<c3><a5>", "bad<ff>", NA_character_)
  y <- .decode_escaped_bytes(x[!is.na(x)])
  expect_identical(y[1:4], c("blåkveite", "sølvkveite", "torsk", "ærfuglå"))
  expect_identical(y[5], "bad<ff>")
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
