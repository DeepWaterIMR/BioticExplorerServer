#' @title Download and parse NMD data for the BioticExplorer database
#' @description Downloads annual NMD data from the API and writes them as a DuckDB database
#' @param connection Object defining the \link[duckdb]{duckdb} connection. Typically made within \code{\link{compileDatabase}}.
#' @param icesAreas ICES area shape \code{\link[sf]{st_polygon}} abject. Used for calculating the ICES area for a specific fishstation.
#' @param cruiseSeries a data.table object of NMD cruise series list. Used to identify cruise series of a specific mission. See \code{\link{prepareCruiseSeriesList}}.
#' @param gearCodes a data.table object of NMD gear code list. Used to make gearname and gearcategory columns. See \code{\link{prepareGearList}}.
#' @inheritParams compileDatabase
#' @details The function downloads NMD data from the API per year, saves these in temp files, reformats them for the DuckDB and writes them into the database. 
#' @import data.table DBI
#' @importFrom utils head
#' @author Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
#' @export

# years = 1914; connection = con_db; icesAreas = icesAreas; cruiseSeries = cruiseSeries; gearCodes = gearCodes; overwrite = FALSE
downloadDatabase <- function(years, connection, icesAreas = icesAreas, cruiseSeries = cruiseSeries, gearCodes = gearCodes, overwrite = FALSE) {
  
  timeStart <- Sys.time()
  
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
      message("Data for ", h, " found. Did not re-download.")
    }
  })
  
  timeEnd <- Sys.time()
  
  DBI::dbWriteTable(connection, "metadata", data.frame(timestart = as.character(timeStart), timeend = as.character(timeEnd)), transaction = FALSE, overwrite = TRUE)
}
