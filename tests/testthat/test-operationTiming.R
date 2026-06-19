test_that("operation timers print timestamps and elapsed minutes", {
  expect_message(
    started_at <- BioticExplorerServer:::.start_operation_timer("Update"),
    "^Update started: [0-9]{4}-[0-9]{2}-[0-9]{2}"
  )
  expect_s3_class(started_at, "POSIXct")

  two_minutes_ago <- Sys.time() - 120
  messages <- capture_messages(
    BioticExplorerServer:::.finish_operation_timer("Update", two_minutes_ago)
  )
  expect_match(messages[[1]], "Update finished successfully: [0-9]{4}-")
  expect_match(messages[[2]], "Update took 2\\.00 minutes\\.")
})
