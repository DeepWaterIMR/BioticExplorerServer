#' @title Compile BioticExplorer database
#' @description Downloads, formulates and indexes BioticExplorer database.
#' @param years vector of integer specifying the years to be downloaded. The database reaches 1900:2020
#' @param dbPath Character string specifying the file path where the database should be located. Must include \code{.monetdb} at the end.
#' @param dbIndexPath Character string specifying the file path where the database should be saved. Must include \code{.rda} at the end.
#' @import data.table
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @export

# dbPath = "~/Desktop/IMR_db.monetdb"; years = 1900:data.table::year(Sys.time()); dbIndexPath = "~/Desktop/dbIndex.rda"
compileDatabase <- function(years = 1900:data.table::year(Sys.time()), dbPath = "~/Desktop/IMR_db.monetdb", dbIndexPath = "~/Desktop/dbIndex.rda") {
  
  downloadDatabase(years = years, dbPath = dbPath)
  indexDatabase(dbPath = dbPath, dbIndexPath = dbIndexPath)
  
}
