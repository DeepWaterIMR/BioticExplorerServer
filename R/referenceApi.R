.REFERENCE_API_BASE <- "https://reference-api.hi.no/apis/nmdapi/reference/v2"

# The Reference API occasionally drops an otherwise healthy TLS connection
# while a large reference list is being assembled.  Keep all reference reads
# behind one client so both compileDatabase() and updateDatabase() retry those
# transient failures consistently.
.reference_api_get_xml <- function(url) {
  request <- httr2::request(url)
  request <- httr2::req_retry(
    request,
    max_tries = 4,
    retry_on_failure = TRUE
  )
  request <- httr2::req_timeout(request, 60)
  response <- httr2::req_perform(request)
  httr2::resp_check_status(response)
  httr2::resp_body_xml(response, check_type = FALSE)
}
