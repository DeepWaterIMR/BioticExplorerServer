#' @title Download and parse NMD data for the BioticExplorer database
#' @description Downloads annual NMD data from the API and writes them as MonetDB database
#' @param years vector of integer specifying the years to be downloaded. The database reaches 1914:2020
#' @param dbPath Character string specifying the file path where the database should be located. Must include \code{.monetdb} at the end.
#' @details This function is scarily powerful. Do not run a large number of years unless you think you know what you are doing
#' @import data.table DBI MonetDBLite
#' @importFrom utils download.file
#' @author Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
#' @export

downloadDatabase <- function(years, dbPath) {

  con_duck <- DBI::dbConnect(MonetDBLite::MonetDBLite(), dbPath)

  # Get ICES area shape
  poly <- prepareICESareas()

  # Get cruise series list
  cruise_series <- prepareCruiseSeriesList()
  cruise_series_lean <- copy(cruise_series)
  cruise_series_lean[, name:= NULL]

  timeStart <- Sys.time()
  # h <- years[[20]]
  lapply(years, function(h) {
    message(paste("Downloading:", h))

    dest <- tempfile(fileext=".xml")
    url <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/", h, "/cache?version=3.0")
    status <- suppressMessages(suppressWarnings(try(utils::download.file(url, dest), silent = TRUE)))

    if(class(status) == "try-error") {
      message(paste("Year", h, "not found from the database. Skipping..."))
    } else {

      # Do transformations

      a <- bioticToDatabase(dest, missionidPrefix = h, icesAreaShape = poly, cruiseSeries = cruise_series_lean)


      lapply(names(a), function(i) {
        message(paste("Parsing", i))

        if(nrow(a[[i]]) > 0) {
          DBI::dbWriteTable(con_duck, i, a[[i]], append=T)
          }
      })

      message(paste("Year", h, "is parsed successfully."))
      unlink(dest)
    }

  })

  timeEnd <- Sys.time()
  
  DBI::dbWriteTable(con_duck, "metadata", data.frame(timestart = timeStart, timeend = timeEnd), overwrite = TRUE)
  DBI::dbWriteTable(con_duck, "csindex", cruise_series, overwrite = TRUE)  

  DBI::dbDisconnect(con_duck)

}
