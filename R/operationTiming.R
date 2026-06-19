.start_operation_timer <- function(operation) {
  started_at <- Sys.time()
  message(
    operation, " started: ",
    format(started_at, "%Y-%m-%d %H:%M:%S %Z")
  )
  started_at
}

.finish_operation_timer <- function(operation, started_at) {
  finished_at <- Sys.time()
  message(
    operation, " finished successfully: ",
    format(finished_at, "%Y-%m-%d %H:%M:%S %Z")
  )
  message(sprintf(
    "%s took %.2f minutes.",
    operation,
    as.numeric(difftime(finished_at, started_at, units = "mins"))
  ))
  invisible(finished_at)
}
