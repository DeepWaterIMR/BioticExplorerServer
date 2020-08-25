#' @title Download and parse NMD data for the BioticExplorer database
#' @description Downloads annual NMD data from the API and writes them as MonetDB database
#' @param years vector of integer specifying the years to be downloaded. The database reaches 1914:2020
#' @param dbPath Character string specifying the file path where the database should be located. Must include \code{.monetdb} at the end.
#' @param icesAreaShape ICES area shape in SpatialPolygonsDataFrame object. Used for calculating the ICES area for a specific fishstation.
#' @param cruiseSeries a data.table object of NMD cruise series list. Used to identify cruise series of a specific mission. See \code{\link{prepareCruiseSeriesList}}.
#' @param gearCodes a data.table object of NMD gear code list. Used to make gearname and gearcategory columns. See \code{\link{prepareGearList}}.
#' @details This function is scarily powerful. Do not run a large number of years unless you think you know what you are doing
#' @import data.table DBI MonetDBLite
#' @importFrom utils download.file
#' @author Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
#' @export

downloadDatabase <- function(years, dbPath, icesAreaShape = ICESareas, cruiseSeries = cruiseSeriesList, gearCodes = gearList) {

  con_duck <- DBI::dbConnect(MonetDBLite::MonetDBLite(), dbPath)

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
      
      filesize <- data.table::data.table(dbyear = h, filesize = file.info(dest)$size)
      DBI::dbWriteTable(con_duck, "filesize", filesize, append = TRUE)
      
      # Do transformations

      a <- bioticToDatabase(dest, missionidPrefix = h, icesAreaShape = icesAreaShape, cruiseSeries = cruiseSeries, gearCodes = gearCodes)

      lapply(names(a), function(i) {
        message(paste("Parsing", i))

        if(nrow(a[[i]]) > 0) {
          DBI::dbWriteTable(con_duck, i, a[[i]], append = TRUE)
        }
      })

      message(paste("Year", h, "is parsed successfully."))
      unlink(dest)
    }

  })

  timeEnd <- Sys.time()
  
  DBI::dbWriteTable(con_duck, "metadata", data.frame(timestart = timeStart, timeend = timeEnd), overwrite = TRUE)
  DBI::dbWriteTable(con_duck, "csindex", cruiseSeries, overwrite = TRUE)  
  DBI::dbWriteTable(con_duck, "gearindex", gearCodes, overwrite = TRUE)
  
  DBI::dbDisconnect(con_duck)

}
