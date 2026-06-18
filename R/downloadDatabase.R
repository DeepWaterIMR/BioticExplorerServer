#' @title Download and parse NMD data for the BioticExplorer database
#' @description Downloads annual NMD data from the API and writes them as a DuckDB database
#' @param connection Object defining the \link[duckdb]{duckdb} connection. Typically made within \code{\link{compileDatabase}}.
#' @param icesAreas ICES area shape \code{\link[sf]{st_polygon}} abject. Used for calculating the ICES area for a specific fishstation.
#' @param cruiseSeries a data.table object of NMD cruise series list. Used to identify cruise series of a specific mission. See \code{\link{prepareCruiseSeriesList}}.
#' @param gearCodes a data.table object of NMD gear code list. Used to make gearname and gearcategory columns. See \code{\link{prepareGearList}}.
#' @param taxaList a data.table object of NMD taxa reference data. Written to the \code{taxaindex} DuckDB table. See \code{\link{prepareTaxaList}}.
#' @inheritParams compileDatabase
#' @details The function writes the taxa reference list to the database, downloads NMD data from the API per year, saves these in temp files, reformats them for the DuckDB and writes them into the database.
#' @return Called for its side effects: appends parsed Biotic data to the DuckDB database. Returns \code{NULL} invisibly.
#' @import data.table DBI
#' @importFrom utils head
#' @author Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
#' @export

# years = 1914; connection = con_db; icesAreas = icesAreas; cruiseSeries = cruiseSeries; gearCodes = gearCodes; taxaList = taxaList; overwrite = FALSE
downloadDatabase <- function(years, connection, icesAreas = NULL, cruiseSeries = NULL, gearCodes = NULL, taxaList = NULL, overwrite = FALSE) {
  
  timeStart <- Sys.time()

  if (
    inherits(try(dplyr::tbl(connection, "taxaindex"), silent = TRUE), "try-error") |
      overwrite
  ) {
    if (is.null(taxaList)) {
      taxaList <- prepareTaxaList()
    }

    DBI::dbWriteTable(connection, "taxaindex", taxaList, overwrite = overwrite)
  }
  
  # h <- years[[1]]
  lapply(years, function(h) {
    
    if (!.database_has_year(connection, h) || overwrite) {
      
      message(paste("Downloading:", h))
      
      dest <- tempfile(fileext = ".xml")
      url <- paste0("https://biotic-api.hi.no/apis/nmdapi/biotic/v3/", h, "/cache?version=3.1")
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
            message("Download still failing. Trying once more...")
            status <- suppressMessages(
              suppressWarnings(try(utils::download.file(url, dest), silent = TRUE))
            )
          }

          if(inherits(status, "try-error")) {
            message("Still no success. Trying once more...")
            status <- suppressMessages(
              suppressWarnings(try(utils::download.file(url, dest), silent = TRUE))
            )
          }

          if(inherits(status, "try-error")) {
            if(!is.na(file.info(dest)$size)) {
              stop("Giving up after 4 attempts. Could this be a download timeout? Current timeout: ", getOption('timeout'), " seconds. Set a higher limit with options(timeout = ...) and try again.")
            } else {
              message(paste("Year", h, "not found from the database. Skipping..."))
            }
          } else {
            message("Redownload succeeded!")
          }
        }
      } 
      
      if(!skip) {
        # Do transformations
        
        a <- bioticToDatabase(dest, missionidPrefix = h, icesAreas = icesAreas, cruiseSeries = cruiseSeries, gearCodes = gearCodes)

        message("Writing year ", h, if (overwrite) " (replacing existing data)" else "")
        .write_database_year(
          connection = connection,
          year = h,
          parsed = a,
          filesize = file.info(dest)$size,
          replace = overwrite
        )
        
        message(paste("Year", h, "is parsed successfully."))
        unlink(dest)
      }
    } else {
      message("Data for ", h, " found. Did not re-download.")
    }
  })
  
  timeEnd <- Sys.time()
  
  .write_database_metadata(connection, timeStart, timeEnd, update_mode = "compile")
}
