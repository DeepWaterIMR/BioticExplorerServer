#' @title Read and process a NMD Biotic xml file for further use in the BioticExplorer database
#' @description A wrapper for \code{\link[RstoxData]{readXmlFile}} to enable further use in the BioticExplorer database
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param removeEmpty logical indicating whether empty columns should be removed from the output. 
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function, but leads to problems with non-unicode characters.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @param missionidPrefix A prefix for the \code{missionid} identifier, which separates cruises. Used when several xml files are put together. \code{NULL} (default) omits the prefix.
#' @inheritParams downloadDatabase
#' @return Returns a list of Biotic data with \code{$mission}, \code{$stnall} and \code{$indall} data tables. The \code{$stnall} and \code{$indall} are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). 
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research) 
#' @import data.table
#' @export


# Debugging parameters
# removeEmpty = FALSE; convertColumns = FALSE; returnOriginal = FALSE; missionidPrefix = NULL
# file = dest; missionidPrefix = h; icesAreaShape = icesAreaShape;
bioticToDatabase <- function(file, removeEmpty = FALSE, convertColumns = FALSE, returnOriginal = FALSE, missionidPrefix = NULL, icesAreas = icesAreas, cruiseSeries = cruiseSeries, gearCodes = gearCodes) {
  
  pb <- utils::txtProgressBar(max = 10, style = 3)
  
  ## Checks
  
  if(!file.exists(file)) stop("file does not exist. Check your file path.")
  
  utils::setTxtProgressBar(pb, 1)
  
  ## Read the Biotic file ----
  
  dt <- RstoxData::readXmlFile(file)
  
  utils::setTxtProgressBar(pb, 2)
  
  ## Mission data ---
  
  msn <- dt$mission
  
  if (is.null(missionidPrefix)) {
    msn$missionid <- rownames(msn)
  } else {
    msn$missionid <- paste(missionidPrefix, rownames(msn), sep = "_")
  }
  
  ### Add cruise series information
  
  msn <- merge(msn, cruiseSeries[,!names(cruiseSeries) %in% c("name"), with = FALSE], by = c("startyear", "platformname", "cruise"), all.x = TRUE, sort = FALSE)
  ints <- intersect(names(dt$mission), names(dt$fishstation))
  msn[, cruiseseriescode := as.character(cruiseseriescode)]
  msn <- msn[, cruiseseriescode := paste(cruiseseriescode, collapse =","), by = ints]
  msn <- msn[!duplicated(msn[, ..ints]),]
  
  ### Convert dates
  
  if (convertColumns) {
    date.cols <- grep("date", names(msn), value = TRUE)
    msn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  utils::setTxtProgressBar(pb, 3)
  
  ## Station data ---
  
  stn <- dt$fishstation
  
  stn[is.na(stationstarttime), stationstarttime := "00:00:00.000Z"]
  stn[is.na(stationstoptime), stationstoptime := "00:00:00.000Z"]
  
  stn[, stationstartdate := as.POSIXct(paste(stn$stationstartdate, stn$stationstarttime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]
  stn[, stationstopdate := as.POSIXct(paste(stn$stationstopdate, stn$stationstoptime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]
  
  stn[, stationstarttime := NULL]
  stn[, stationstoptime := NULL]
  
  ### Add ICES area
  
  if(!is.null(icesAreas)) {
    
    ## Turn s2 off
    s2_mode <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit({suppressMessages(sf::sf_use_s2(s2_mode))})
    
    points <- stn[, c("longitudestart", "latitudestart")]
    
    # Remove NAs (set longlat as 0 so that translation gives NA)
    points[is.na(longitudestart) | is.na(latitudestart), `:=`(longitudestart=0, latitudestart = 0)]
    
    if(nrow(points) > 0) {
      points <- sf::st_as_sf(points, coords = c(1,2), crs = 4326)
      suppressWarnings(sf::st_crs(icesAreas) <- 4326)
      points <- sf::st_transform(points, sf::st_crs(icesAreas))
      
      stn[, icesarea := icesAreas[as.integer(suppressMessages(sf::st_intersects(points, icesAreas))),]$Area_Full]
    } else {
      stn[, icesarea := as.character(NA)]
    }
  }
  
  ### Fix FDIR area code
  
  stn[, area := as.integer(area)]
  
  ### Add gear category
  
  stn <- merge(stn, gearCodes[,!names(gearCodes) %in% c("description"), with = FALSE], by.x = c("gear"), by.y = c("code"), all.x = TRUE, sort = FALSE)
  
  ### Convert date columns
  
  # if (convertColumns) { # Fixes the time issue. Left here in case unforeseen consequences. 
  #   #date.cols <- grep("date", names(stn), value = TRUE)
  #   #stn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  # }
  
  utils::setTxtProgressBar(pb, 4)
  
  ##________________
  ## Sample data ---
  
  cth <- dt$catchsample
  
  ##____________________
  ## Individual data ---
  
  ind <- dt$individual
  utils::setTxtProgressBar(pb, 5)
  
  ## Age data ---
  
  age <- dt$agedetermination
  
  if (convertColumns) {
    date.cols <- grep("date", names(age), value = TRUE)
    age[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  utils::setTxtProgressBar(pb, 6)
  
  # if (nrow(age) == 0) {
  #   age <- rapply(age, as.integer, how = "replace")
  # }
  
  ## Compiled datasets ----
  
  coredat <- merge(msn[, setdiff(names(msn), c("purpose")), with = FALSE], stn, by = intersect(names(msn), names(stn)), all = TRUE)
  
  utils::setTxtProgressBar(pb, 7)
  
  # Stndat
  
  stndat <- merge(coredat, cth, all.y = TRUE, by = c("missiontype", "missionnumber", "startyear", "platform", "serialnumber"))
  stndat[is.na(commonname), commonname := "Empty"]
  
  utils::setTxtProgressBar(pb, 8)
  
  # Inddat
  
  inddat <- merge(stndat[, setdiff(names(stndat), c("purpose", "stationcomment", "catchcomment")), with = FALSE], ind, all.y = TRUE, by = intersect(names(stndat), names(ind)))
  
  age[,numberofreads:=length(age),.(startyear,platform,serialnumber,catchsampleid,specimenid)]
  
  # Agedat
  
  agedat <- merge(inddat, age, by = intersect(names(inddat), names(age)), all.y = T)
  
  # More inddat
  
  inddat[is.na(preferredagereading), preferredagereading := 1]
  
  inddat <- merge(inddat, age, by.x = c(intersect(names(inddat), names(age)), "preferredagereading"), by.y = c(intersect(names(inddat), names(age)), "agedeterminationid"), all.x = TRUE)
  
  if(sum(is.na(inddat$commonname)) > 0) stop(paste(sum(is.na(inddat$commonname)), "missing commonname records. This is likely due to merging error between individual and agedetermination data tables. File a bug report."))
  
  utils::setTxtProgressBar(pb, 9)
  
  ## Return ----
  
  ### Format
  
  if (returnOriginal) {
    out <- list(mission = msn, fishstation = stn, catchsample = cth, individual = ind, agedetermination = age, stnall = stndat, indall = inddat, ageall = agedat)
  } else {
    out <- list(mission = msn, stnall = stndat, indall = inddat, ageall = agedat)
  }
  
  utils::setTxtProgressBar(pb, 10)
  
  ### Remove empty columns to save space
  
  if (removeEmpty) {
    out <- lapply(out, function(k) {
      k[, which(unlist(lapply(k, function(x) !all(is.na(x))))), with = FALSE]
    })
  }
  
  utils::setTxtProgressBar(pb, 11)
  
  ### Class
  
  class(out) <- "bioticProcData"
  
  ### Return
  
  out
  
}


#' @title Print processed NMD Biotic data (\code{bioticProcData}) objects
#' @description \code{\link{print}} function for \code{\link[=bioticToDatabase]{bioticProcData}} objects
#' @param x \code{bioticProcData} object to be printed.
#' @param ... further arguments passed to \code{\link{print}}.
#' @method print bioticProcData
#' @author Mikko Vihtakari
#' @seealso \code{\link{bioticToDatabase}}
#' @export

print.bioticProcData <- function(x, ...) {
  
  cat("Processed Biotic Data object")
  cat(paste(" of class", class(x)), sep = "\n")
  cat(NULL, sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("$mission: ", nrow(x$mission), " rows and ", ncol(x$mission), " columns"), sep = "\n")
  cat(paste0("$stnall: ", nrow(x$stnall), " rows and ", ncol(x$stnall), " columns"), sep = "\n")
  cat(paste0("$indall: ", nrow(x$indall), " rows and ", ncol(x$indall), " columns"), sep = "\n")
  cat(paste0("$ageall: ", nrow(x$ageall), " rows and ", ncol(x$ageall), " columns"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Object size: ", sep = "")
  print(utils::object.size(x), unit = "auto")
  cat("Years: ", sep = "")
  cat(unique(x$mission$startyear), sep = ", ")
  cat(NULL, sep = "\n")
  cat(paste0(length(unique(x$mission$cruise)), " cruises, ", length(unique(paste(x$stnall$startyear, x$stnall$serialnumber))), " separate stations and ", nrow(x$indall), " measured fish."), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("Geographic range: ", 
             round(suppressWarnings(min(x$stnall$longitudestart, na.rm = TRUE)), 1), "-", 
             round(suppressWarnings(max(x$stnall$longitudestart, na.rm = TRUE)), 1), " degrees longitude and ", 
             round(suppressWarnings(min(x$stnall$latitudestart, na.rm = TRUE)), 1), "-", 
             round(suppressWarnings(max(x$stnall$latitudestart, na.rm = TRUE)), 1), " latitude."), sep = "\n")
  cat("Number of missing station coordinates: ", sep = "")
  cat(sum(is.na(x$stnall$longitudestart) | is.na(x$stnall$latitudestart)))
  cat(NULL, sep = "\n")
  cat("Unique species: ", sep = "")
  cat(sort(unique(x$stnall$commonname)), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  
}
