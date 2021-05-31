#' @title Compile BioticExplorer database
#' @description Downloads, formulates and indexes BioticExplorer database.
#' @inheritParams downloadDatabase
#' @param dbIndexPath Character string specifying the file path where the database should be saved. Must include \code{.rda} at the end.
#' @param source Character string indicating from where the database should be compiled. If \code{NULL}, the data will be downloaded from NMD. Requires access to the IMR intranet. Otherwise, specify a file path where the rds files are located from the \code{\link{downloadDatabaseToFiles}} function. NOT IMPLEMENTED YET.
#' @details Runs the \code{\link{downloadDatabase}} and \code{\link{indexDatabase}} functions. Be aware that running these functions requires access to the IMR intranet, reasonably fast internet and loads of memory. If the function crashes after the \code{\link{downloadDatabase}}, you can still run the \code{\link{indexDatabase}} to save the progress. If it crashes during \code{\link{downloadDatabase}}, you may have to start from scratch. 
#' @import data.table
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @export

compileDatabase <- function(years = 1900:data.table::year(Sys.time()), dbIndexPath = "~/Desktop/dbIndex.rda", source = NULL, dbName = NULL) {

  ## Cruise series
  
  message("1. Compiling cruise series list")
  cruiseSeriesList <- prepareCruiseSeriesList()
  # data(cruiseSeriesList) # a debugging shortcut. Works if you update the data/cruiseSeriesList.rda first. 
  
  ## Gear list
  
  message("2. Compiling gear list")
  gearList <- prepareGearList()
  
  ## ICES Areas

  message("3. Compiling ICES area shapes")
  # if(!exists("icesAreas")) data(icesAreas)

  ## Download
  
  message("4. Compiling database")
  if(is.null(source)) {
    downloadDatabase(years = years, icesAreaShape = get(icesAreas), cruiseSeries = cruiseSeriesList, gearCodes = gearList, dbName = dbName)
  } else {
    stop("not implemented yet")
  }
  
  # Index
  
  message("4. Indexing database")
  indexDatabase(dbIndexPath = dbIndexPath, dbName = dbName)
  
}
