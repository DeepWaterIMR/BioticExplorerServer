#' @title Index BioticExplorer database
#' @description Loads BioticExplorer database and creates an index used by BioticExplorer to save processing time
#' @param dbPath Character string specifying the file path where the database should be located. Must include \code{.monetdb} at the end.
#' @param dbIndexPath Character string specifying the file path where the database should be saved. Must include \code{.rda} at the end.
#' @import data.table DBI MonetDBLite dtplyr
#' @rawNamespace import(dplyr, except = c(last, first, between))
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)

indexDatabase <- function(dbPath = "~/Desktop/IMR_db.monetdb", dbIndexPath = "~/Desktop/dbIndex.rda") {
  
  con_db <- DBI::dbConnect(MonetDBLite::MonetDBLite(), dbPath)
  
  rv <- list()
  rv$inputData$stnall <- dplyr::tbl(con_db, "stnall")
  rv$inputData$indall <- dplyr::tbl(con_db, "indall")
  rv$inputData$mission <- dplyr::tbl(con_db, "mission")
  
  ## Index
  
  index <<- list()
  index$missiontypename <- rv$inputData$mission %>% lazy_dt() %>% select(missiontypename) %>% distinct() %>% pull() %>% sort()
  index$cruise <- rv$inputData$mission %>% lazy_dt() %>% select(cruise) %>% distinct() %>% pull() %>% sort()
  index$year <- rv$inputData$mission %>% lazy_dt() %>% select(startyear) %>% distinct() %>% pull() %>% sort()
  index$nstations <- rv$inputData$stnall %>% lazy_dt() %>% select(missionid, startyear, serialnumber) %>% distinct() %>% count() %>% pull()
  index$commonname <- rv$inputData$stnall %>% lazy_dt() %>% select(commonname) %>% distinct() %>% pull() %>% sort()
  index$platformname <- rv$inputData$stnall %>% lazy_dt() %>% select(platformname) %>% distinct() %>% pull() %>% sort()
  index$serialnumber <- rv$inputData$stnall %>% lazy_dt() %>% select(serialnumber) %>% distinct() %>% pull() %>% sort()
  index$gear <- rv$inputData$stnall %>% lazy_dt() %>% select(gear) %>% distinct() %>% pull() %>% sort()
  index$date <- rv$inputData$stnall %>% lazy_dt() %>% summarise(min = min(stationstartdate, na.rm = TRUE), max = max(stationstartdate, na.rm = TRUE)) %>% collect()
  # index$nmeasured <- rv$inputData$indall %>% lazy_dt() %>% select(length) %>% count() %>% pull()
  
  # print(object.size(DBI::dbReadTable(con_db, "stndat")), units = "GB")
  
  ## Close database connection
  
  MonetDBLite::monetdblite_shutdown()
  
  ## Save and return
  
  save(index, file = dbIndexPath, compress = "xz")
  
  index
}
