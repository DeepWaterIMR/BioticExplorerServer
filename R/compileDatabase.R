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
compileDatabase <- function(years = 1900:data.table::year(Sys.time()), dbIndexPath = "~/Desktop/dbIndex.rda", source = NULL, dbName = NULL, prepareCruiseSeries = TRUE) {

  ## Define dbName and dbIndexPath
  
  if(Sys.getenv(c("SERVER_MODE"))=="") {
    dbHost <- "localhost"
    if(is.null(dbName)) dbName <- "bioticexplorer"
  } else {
    dbHost <- "dbserver"
    if(is.null(dbName)) dbName <- "bioticexplorer-next"
  }
  
  dbIndexPath <- file.path(dbIndexPath, paste0(dbName, ".rda"))
  
  ## Cruise series
  
  message("1. Compiling cruise series list")
  if(prepareCruiseSeries) {
    cruiseSeries <- prepareCruiseSeriesList()
  } 
  
  ## Gear list
  
  message("2. Compiling gear list")
  gearCodes <- prepareGearList()
  
  ## Download
  
  message("3. Compiling database")
  if(is.null(source)) {
    downloadDatabase(years = years, icesAreas = icesAreas, cruiseSeries = cruiseSeries, gearCodes = gearCodes, dbName = dbName)
  } else {
    stop("not implemented yet")
  }
  
  # Index
  
  message("4. Indexing database")
  indexDatabase(dbIndexPath = dbIndexPath, dbName = dbName)
  
}
