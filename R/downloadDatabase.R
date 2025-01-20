#' @title Download and parse NMD data for the BioticExplorer database
#' @description Downloads annual NMD data from the API and writes them as DuckDB database
#' @param years vector of integer specifying the years to be downloaded. The database reaches 1914:2020
#' @param icesAreas ICES area shape \code{\link[sf]{st_polygon}} abject. Used for calculating the ICES area for a specific fishstation.
#' @param cruiseSeries a data.table object of NMD cruise series list. Used to identify cruise series of a specific mission. See \code{\link{prepareCruiseSeriesList}}.
#' @param gearCodes a data.table object of NMD gear code list. Used to make gearname and gearcategory columns. See \code{\link{prepareGearList}}.
#' @param dbName Character string or \code{NULL}. If \code{NULL} uses the default names and overwrites the existing database. 
#' @param overwrite Logical indicating whether existing information in the \code{dbName} should be downloaded again and overwritten.
#' @details The function downloads NMD data from the API per year, saves these in temp files, reformats them for the MonetDB and writes them into the database. Server mode (Eucleia docker or local) is automatically detected. Requires MonetDB installed and running on the computer.
#' @import data.table DBI
#' @importFrom utils head
#' @author Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
#' @export

# years = 1914; connection = con_db; icesAreas = icesAreas; cruiseSeries = cruiseSeries; gearCodes = gearCodes; overwrite = FALSE


downloadDatabase <- function(years, connection, icesAreas = icesAreas, cruiseSeries = cruiseSeries, gearCodes = gearCodes, overwrite = FALSE) {
  
  timeStart <- Sys.time()
  
  # if(inherits(try(dplyr::tbl(connection, "csindex"), silent = TRUE), "try-error") | overwrite) {
  #   DBI::dbWriteTable(connection, "csindex", cruiseSeries, csvdump = TRUE,
  #                     transaction = FALSE, overwrite = TRUE)
  # }
  # 
  # if(inherits(try(dplyr::tbl(connection, "gearindex"), silent = TRUE), "try-error") | overwrite) {
  #   DBI::dbWriteTable(connection, "gearindex", gearCodes, csvdump = TRUE, 
  #                     transaction = FALSE, overwrite = TRUE)
  # }
  
  # h <- years[[1]]
  lapply(years, function(h) {
    
    tmp <- try(dplyr::filter(dplyr::tbl(connection, "stnall"), startyear == h), silent = TRUE)
    
    if(ifelse(inherits(tmp, "try-error"), TRUE, 
              length(dplyr::pull(head(tmp), startyear)) == 0) | overwrite) {
      
      message(paste("Downloading:", h))
      
      dest <- tempfile(fileext = ".xml")
      url <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/", h, "/cache?version=3.1")
      status <- suppressMessages(
        suppressWarnings(try(utils::download.file(url, dest), silent = TRUE))
      )
      skip <- FALSE
      
      if(inherits(status, "try-error")) {
        if(is.na(file.info(dest)$size)) {
          message(paste("Year", h, "not found from the database. Skipping..."))
          skip <- TRUE
        } else {
          
          message("Possible connection error. Trying to download again")
          status <- suppressMessages(
            suppressWarnings(try(utils::download.file(url, dest), silent = TRUE))
          )
          
          if(inherits(status, "try-error")) {
            message("Download still failing. Shit connection to IMR servers; sitting in Tromsoe perhaps?. Trying once more...")
            status <- suppressMessages(
              suppressWarnings(try(utils::download.file(url, dest), silent = TRUE))
            ) 
          }
          
          if(inherits(status, "try-error")) {
            message("Still no success. Your internet could also suck? Trying once more...")
            status <- suppressMessages(
              suppressWarnings(try(utils::download.file(url, dest), silent = TRUE))
            ) 
          }
          
          if(inherits(status, "try-error")) {
            if(!is.na(file.info(dest)$size)) {
              stop("Giving up...redownload failed. Could this be download timeout error? Current timeout ", getOption('timeout'),". Set a higher timeout limit using options(timeout = ...), or try again. Maybe it was just bad server connection as so often.")
            } else {
              message("Olet spede! Painu vittuun.")
            }
          } else {
            message("Redownload succeeded!")
          }
        }
      } 
      
      if(!skip) {
        filesize <- data.table::data.table(dbyear = h, filesize = file.info(dest)$size)
        DBI::dbWriteTable(connection, "filesize", filesize, transaction = FALSE, append = TRUE)
        
        # Do transformations
        
        a <- bioticToDatabase(dest, missionidPrefix = h, icesAreas = icesAreas, cruiseSeries = cruiseSeries, gearCodes = gearCodes)
        
        lapply(names(a), function(i) {
          message(paste("Parsing", i))
          
          if(nrow(a[[i]]) > 0) {
            DBI::dbWriteTable(connection, i, a[[i]], csvdump = TRUE, transaction = FALSE, append = TRUE)
          }
        })
        
        message(paste("Year", h, "is parsed successfully."))
        unlink(dest)
      }
    } else {
      message("Data for ", h, " found from ", dbName, ". Did not re-download.")
    }
  })
  
  timeEnd <- Sys.time()
  
  DBI::dbWriteTable(connection, "metadata", data.frame(timestart = as.character(timeStart), timeend = as.character(timeEnd)), transaction = FALSE, overwrite = TRUE)
  
  # DBI::dbDisconnect(connection)
}
