#' @title Compile BioticExplorer database
#' @description Downloads, formulates and indexes BioticExplorer database.
#' @inheritParams downloadDatabase
#' @param dbIndexPath Character string specifying the file path where the database should be saved. Must include \code{.rda} at the end.
#' @param source Character string indicating from where the database should be compiled. If \code{NULL}, the data will be downloaded from NMD. Requires access to the IMR intranet. Otherwise, specify a file path where the rds files are located from the \code{\link{downloadDatabaseToFiles}} function. NOT IMPLEMENTED YET.
#' @param prepareCruiseSeries Logical indicating whether \link[=prepareCruiseSeriesList]{the cruise series list} should be prepared. Only set this to \code{FALSE} while debugging as preparing the cruise series list takes time. 
#' @details Runs the \code{\link{downloadDatabase}} and \code{\link{indexDatabase}} functions. Be aware that running these functions requires access to the IMR intranet, reasonably fast internet and loads of memory. If the function crashes after the \code{\link{downloadDatabase}}, you can still run the \code{\link{indexDatabase}} to save the progress. If it crashes during \code{\link{downloadDatabase}}, you may have to start from scratch. 
#' @import data.table
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @export

# years = 1914; dbIndexPath = "~/Desktop/test.rda"; source = NULL; dbName = "test"; prepareCruiseSeries = FALSE
compileDatabase <- function(
    years = 1900:data.table::year(Sys.time()), dbIndexPath = "~/Desktop/dbIndex.rda", 
    source = NULL, dbName = NULL, prepareCruiseSeries = TRUE, overwrite = FALSE
) {
  
  ## Define dbName and dbIndexPath
  
  if(Sys.getenv(c("SERVER_MODE"))=="") {
    dbHost <- "localhost"
    if(is.null(dbName)) dbName <- "bioticexplorer"
  } else {
    dbHost <- "dbserver"
    if(is.null(dbName)) dbName <- "bioticexplorer-next"
  }
  
  con_db <- 
    try({DBI::dbConnect(MonetDB.R::MonetDB.R(), host=dbHost, dbname=dbName, 
                        user="monetdb", password="monetdb")}, silent = TRUE)
  
  # dbIndexPath <- file.path(dbIndexPath, paste0(dbName, ".rda"))
  
  ## Cruise series
  
  message("1. Compiling cruise series list")
  if(prepareCruiseSeries) {
    if(inherits(try(dplyr::tbl(con_db, "csindex"), silent = TRUE), "try-error") | overwrite) {
      cruiseSeries <- prepareCruiseSeriesList()
    } else {
      cruiseSeries <- dplyr::collect(dplyr::tbl(con_db, "csindex"))
      message("Cruise series information found from ", dbName, 
              ". The information was not rewritten. Delete the database or use the overwrite argument if you want to re-download the data.")
    }
  } 
  
  ## Gear list
  
  message("2. Compiling gear list")
  
  if(inherits(try(dplyr::tbl(con_db, "gearindex"), silent = TRUE), "try-error") | overwrite) {
  gearCodes <- prepareGearList()
  } else {
    gearCodes <- dplyr::collect(dplyr::tbl(con_db, "gearindex"))
    message("Gead codes found from ", dbName, 
            ". The information was not rewritten. Delete the database or use the overwrite argument if you want to re-download the data.")
  }
  
  ## Download
  
  message("3. Compiling database")
  if(is.null(source)) {
    downloadDatabase(years = years, icesAreas = icesAreas, cruiseSeries = cruiseSeries, gearCodes = gearCodes, dbName = dbName, overwrite = overwrite)
  } else {
    stop("not implemented yet")
  }
  
  # Index
  
  message("4. Indexing database")
  indexDatabase(dbIndexPath = dbIndexPath, dbName = dbName)
  
}
