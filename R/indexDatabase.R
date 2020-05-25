#' @title Index BioticExplorer database
#' @description Loads BioticExplorer database and creates an index used by BioticExplorer to save processing time
#' @param dbPath Character string specifying the file path where the database should be located. Must include \code{.monetdb} at the end.
#' @param dbIndexPath Character string specifying the file path where the database should be saved. Must include \code{.rda} at the end.
#' @param fileOnly Logical indicating whether the result should only be saved to a file and not returned. If FALSE, no file is made and the result is returned instead.
#' @import data.table DBI MonetDBLite
#' @rawNamespace import(dplyr, except = c(last, first, between))
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @export

# dbPath = "~/Desktop/IMR_db.monetdb"; dbIndexPath = "~/Desktop/dbIndex.rda"
indexDatabase <- function(dbPath = "~/Desktop/IMR_db.monetdb", dbIndexPath = "~/Desktop/dbIndex.rda", fileOnly = TRUE) {
  
  pb <- utils::txtProgressBar(max = 6, style = 3)
  
  con_db <- DBI::dbConnect(MonetDBLite::MonetDBLite(), dbPath)
  
  utils::setTxtProgressBar(pb, 1)
  
  rv <- list()
  rv$inputData$stnall <- dplyr::tbl(con_db, "stnall")
  rv$inputData$indall <- dplyr::tbl(con_db, "indall")
  rv$inputData$mission <- dplyr::tbl(con_db, "mission")
  rv$inputData$meta <- dplyr::tbl(con_db, "metadata")
  
  utils::setTxtProgressBar(pb, 2)
  
  ## Index
  
  index <- list()
  index$missiontypename <- rv$inputData$mission %>% select(missiontypename) %>% distinct() %>% pull() %>% sort()
  index$cruise <- rv$inputData$mission %>% select(cruise) %>% distinct() %>% pull() %>% sort()
  index$year <- rv$inputData$mission %>% select(startyear) %>% distinct() %>% pull() %>% sort()
  index$nstations <- rv$inputData$stnall %>% select(missionid, startyear, serialnumber) %>% distinct() %>% count() %>% pull()
  
  utils::setTxtProgressBar(pb, 3)
  
  index$commonname <- rv$inputData$stnall %>% select(commonname) %>% distinct() %>% pull() %>% sort()
  index$platformname <- rv$inputData$stnall %>% select(platformname) %>% distinct() %>% pull() %>% sort()
  index$serialnumber <- rv$inputData$stnall %>% select(serialnumber) %>% distinct() %>% pull() %>% sort()
  index$gear <- rv$inputData$stnall %>% select(gear) %>% distinct() %>% pull() %>% sort()
  index$date <- rv$inputData$stnall %>% summarise(min = min(stationstartdate, na.rm = TRUE), max = max(stationstartdate, na.rm = TRUE)) %>% collect()
  
  utils::setTxtProgressBar(pb, 4)
  
  index$nmeasured <- rv$inputData$indall %>% select(length) %>% count() %>% pull()
  
  utils::setTxtProgressBar(pb, 5)
  
  index$downloadstart <- rv$inputData$meta %>% select(timestart) %>% pull()
  index$downloadend <- rv$inputData$meta %>% select(timeend) %>% pull()
  
  # index$stnallsize <- print(object.size(DBI::dbReadTable(con_db, "stnall")), units = "GB")
  # 
  # utils::setTxtProgressBar(pb, 6)
  # 
  # index$indallsize <- print(object.size(DBI::dbReadTable(con_db, "indall")), units = "GB")
  # 
  # utils::setTxtProgressBar(pb, 7)
  # 
  ## Close database connection
  
  DBI::dbDisconnect(con_db) # MonetDBLite::monetdblite_shutdown()
  
  ## Save and return
  
  utils::setTxtProgressBar(pb, 6)
  
  if(fileOnly) {
    save(index, file = dbIndexPath, compress = "xz")
  } else {
    return(index)
  }
}



