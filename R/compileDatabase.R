#' @title Compile BioticExplorer database
#' @description Downloads, formulates and indexes BioticExplorer database.
#' @inheritParams downloadDatabase
#' @param dbIndexPath Character string specifying the file path where the database should be saved. Must include \code{.rda} at the end.
#' @details Runs the \code{\link{downloadDatabase}} and \code{\link{indexDatabase}} functions. Be aware that running these functions requires access to the IMR intranet, reasonably fast internet and loads of memory. If the function crashes after the \code{\link{downloadDatabase}}, you can still run the \code{\link{indexDatabase}} to save the progress. If it crashes during \code{\link{downloadDatabase}}, you may have to start from scratch. 
#' @import data.table
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @export

# years = 1914; dbPath = "~/Desktop/IMR_biotic_BES_database"; dbIndexFile = file.path(dbPath, "dbIndex.rda"); dbName = NULL; overwrite = TRUE
# compileDatabase(years = 1914, dbName = "spedenpatukka", overwrite = TRUE)

compileDatabase <- function(
    years = 1900:data.table::year(Sys.time()), dbPath = "~/Documents/IMR_biotic_BES_database", 
    dbIndexFile = file.path(dbPath, "dbIndex.rda"), dbName = NULL, 
    overwrite = FALSE
) {
  
  ## Create the database folder if it does not exist
  
  if(!dir.exists(dbPath)) {
    message(dbPath, " does not exist. Do you want to create the folder?")
    
    ret.val <- utils::menu(c("Yes", "No"), "")
    
    if(ret.val != 1) {
      msg <- paste0("Selected not to create the folder. Redefine dbPath and try again.")
      stop(paste(strwrap(msg), collapse= "\n"))
    } else {
      dir.create(dbPath)
      msg <- paste0("duckdb IMR database created to ", dbPath)
      message(paste(strwrap(msg), collapse= "\n"))
    }
  }
  
  ## Define dbName and dbIndexPath
  
  if(Sys.getenv(c("SERVER_MODE"))=="") {
    dbHost <- "localhost"
    if(is.null(dbName)) dbName <- "bioticexplorer"
  } else {
    dbHost <- "dbserver"
    if(is.null(dbName)) dbName <- "bioticexplorer-next"
  }
  
  con_db <- 
    try({DBI::dbConnect(
      duckdb::duckdb(
        dbdir = normalizePath(paste0(file.path(dbPath, dbName), ".duckdb"), 
                              mustWork = FALSE)))}, 
      silent = TRUE)
  
  ## Cruise series
  
  message("1. Compiling cruise series list")
  if(inherits(try(dplyr::tbl(con_db, "csindex"), silent = TRUE), "try-error") | overwrite) {
    cruiseSeries <- prepareCruiseSeriesList()
  } else {
    cruiseSeries <- dplyr::collect(dplyr::tbl(con_db, "csindex"))
    message("Cruise series information found from ", dbName, 
            ". The information was not rewritten. Delete the database or use the overwrite argument if you want to re-download the data.")
  }
  
  if(inherits(try(dplyr::tbl(con_db, "csindex"), silent = TRUE), "try-error") | overwrite) {
    DBI::dbWriteTable(con_db, "csindex", cruiseSeries, overwrite = overwrite) #, csvdump = TRUE,
    #transaction = FALSE, overwrite = TRUE)
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
  
  if(inherits(try(dplyr::tbl(con_db, "gearindex"), silent = TRUE), "try-error") | overwrite) {
    DBI::dbWriteTable(con_db, "gearindex", gearCodes, overwrite = overwrite) #csvdump = TRUE, 
    # transaction = FALSE, overwrite = TRUE)
  }
  
  ## Download
  
  message("3. Compiling database")
  downloadDatabase(
      years = years, connection = con_db, icesAreas = icesAreas, 
      cruiseSeries = cruiseSeries, gearCodes = gearCodes, overwrite = overwrite)
  
  # Index
  
  message("4. Indexing database")
  indexDatabase(connection = con_db, dbIndexPath = dbIndexFile)
  
  DBI::dbDisconnect(con_db)
}
