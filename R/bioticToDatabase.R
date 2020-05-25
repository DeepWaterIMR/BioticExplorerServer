#' @title Read and process a NMD Biotic xml file for further use in the BioticExplorer database
#' @description A wrapper for \code{\link[RstoxData]{readXmlFile}} to enable further use in the BioticExplorer database
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param removeEmpty logical indicating whether empty columns should be removed from the output. 
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function, but leads to problems with non-unicode characters.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @param missionidPrefix A prefix for the \code{missionid} identifier, which separates cruises. Used when several xml files are put together. \code{NULL} (default) omits the prefix.
#' @details This function should be identical to the BioticExplorer::processBioticFile function with the exception that \code{removeEmpty} has to be set to FALSE
#' @param icesAreaShape ICES area shape in SpatialPolygonsDataFrame object. Used for calculating the ICES area for a specific fishstation.
#' @param cruiseSeries a data.table object of NMD cruise series list. Used to identify cruise series of a specific mission.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$stnall} and \code{$indall} data tables. The \code{$stnall} and \code{$indall} are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). 
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research) 
#' @import RstoxData data.table
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom sp over coordinates proj4string coordinates<- proj4string<- CRS spTransform
#' @export


# Debugging parameters
# removeEmpty = FALSE; convertColumns = TRUE; returnOriginal = FALSE; missionidPrefix = NULL
bioticToDatabase <- function(file, removeEmpty = FALSE, convertColumns = TRUE, returnOriginal = FALSE, missionidPrefix = NULL, icesAreaShape = NULL, cruiseSeries) {
  
  pb <- utils::txtProgressBar(max = 10, style = 3)
  
  ## Checks
  
  if(!file.exists(file)) stop("file does not exist. Check your file path.")
  
  ## Read the Biotic file ----
  
  dt <- RstoxData::readXmlFile(file)
  
  utils::setTxtProgressBar(pb, 1)
  
  ## Mission data ---
  
  msn <- dt$mission

  # Add cruise series information
  msn <- merge(msn, cruiseSeries, by.x = c("startyear", "platformname", "cruise"), by.y = c("year", "shipName", "cruisenr"), all.x = TRUE)
  ints <- intersect(names(dt$mission), names(dt$fishstation))
  msn[, cruiseseriescode := as.character(cruiseseriescode)]
  msn <- msn[, cruiseseriescode := paste(cruiseseriescode, collapse =","), by = ints]
  msn <- msn[!duplicated(msn[, ..ints]),]

  if (convertColumns) {
    date.cols <- grep("date", names(msn), value = TRUE)
    msn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  if (is.null(missionidPrefix)) {
    msn$missionid <- rownames(msn)
  } else {
    msn$missionid <- paste(missionidPrefix, rownames(msn), sep = "_")
  }
  
  utils::setTxtProgressBar(pb, 2)
  
  ## Station data ---
  
  stn <- dt$fishstation
  
  stn[is.na(stationstarttime), stationstarttime := "00:00:00.000Z"]
  stn[is.na(stationstoptime), stationstoptime := "00:00:00.000Z"]
  
  stn[, stationstartdate := as.POSIXct(paste(stn$stationstartdate, stn$stationstarttime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]
  stn[, stationstopdate := as.POSIXct(paste(stn$stationstopdate, stn$stationstoptime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]
  
  stn[, stationstarttime := NULL]

  ### Add ICES area
  if(!is.null(icesAreaShape)) {
    points <- stn[, c("longitudestart", "latitudestart")]

    # Remove NAs (set longlat as 0 so that translation gives NA)
    points[is.na(longitudestart) | is.na(latitudestart), `:=`(longitudestart=0, latitudestart = 0)]

    if(nrow(points) > 0) {
      sp::coordinates(points) <- c(1,2)
      sp::proj4string(points) <- CRS("+init=epsg:4326")

      transformedPoints <- sp::spTransform(points, proj4string(icesAreaShape))

      stn[, icesarea := sp::over(transformedPoints, icesAreaShape)$Area_Full]
    } else {
      stn[, icesarea := as.character(NA)]
    }
  }

  stn[, stationstoptime := NULL]
  
  if (convertColumns) {
    date.cols <- grep("date", names(stn), value = TRUE)
    stn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  utils::setTxtProgressBar(pb, 3)
  
  ##________________
  ## Sample data ---
  
  cth <- dt$catchsample
  
  ##____________________
  ## Individual data ---
  
  ind <- dt$individual
  utils::setTxtProgressBar(pb, 4)
  
  
  ## Age data ---
  
  age <- dt$agedetermination
  
  if (convertColumns) {
    date.cols <- grep("date", names(age), value = TRUE)
    age[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  utils::setTxtProgressBar(pb, 5)
  
  # if (nrow(age) == 0) {
  #   age <- rapply(age, as.integer, how = "replace")
  # }
  
  ## Compiled datasets ----

  coredat <- merge(msn[, setdiff(names(msn), c("purpose")), with = FALSE], stn, by = intersect(names(msn), names(stn)), all = TRUE)

  utils::setTxtProgressBar(pb, 6)
  
  # Stndat
  
  stndat <- merge(coredat, cth, all.y = TRUE, by = c("missiontype", "missionnumber", "startyear", "platform", "serialnumber"))
  
  utils::setTxtProgressBar(pb, 7)
  
  # Inddat

  inddat <- merge(stndat[, setdiff(names(stndat), c("purpose", "stationcomment", "catchcomment")), with = FALSE], ind, all.y = TRUE, by = intersect(names(stndat), names(ind)))

  inddat[is.na(preferredagereading), preferredagereading := 1]
  inddat <- merge(inddat, age, by.x=c(intersect(names(inddat), names(age)), "preferredagereading"), by.y= c(intersect(names(inddat), names(age)), "agedeterminationid"), all.x = TRUE)
  
  if(sum(is.na(inddat$commonname)) > 0) stop(paste(sum(is.na(inddat$commonname)), "missing commonname records. This is likely due to merging error between individual and agedetermination data tables. File a bug report."))
  
  utils::setTxtProgressBar(pb, 8)
  
  ## Return ----
  
  ### Format
  
  if (returnOriginal) {
    out <- list(mission = msn, fishstation = stn, catchsample = cth, individual = ind, agedetermination = age, stnall = stndat, indall = inddat)
  } else {
    out <- list(mission = msn, stnall = stndat, indall = inddat)
  }
  
  utils::setTxtProgressBar(pb, 9)
  
  ### Remove empty columns to save space
  
  if (removeEmpty) {
    out <- lapply(out, function(k) {
      k[, which(unlist(lapply(k, function(x) !all(is.na(x))))), with = FALSE]
    })
  }
  
  utils::setTxtProgressBar(pb, 10)
  
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
  cat(NULL, sep = "\n")
  cat("Object size: ", sep = "")
  print(utils::object.size(x), unit = "auto")
  cat("Years: ", sep = "")
  cat(unique(x$mission$startyear), sep = ", ")
  cat(NULL, sep = "\n")
  cat(paste0(length(unique(x$mission$cruise)), " cruises, ", length(unique(paste(x$stnall$startyear, x$stnall$serialnumber))), " separate stations and ", nrow(x$indall), " measured fish."), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("Geographic range: ", round(min(x$stnall$longitudestart, na.rm = TRUE), 1), "-", round(max(x$stnall$longitudestart, na.rm = TRUE), 1), " degrees longitude and ", round(min(x$stnall$latitudestart, na.rm = TRUE), 1), "-", round(max(x$stnall$latitudestart, na.rm = TRUE), 1), " latitude."), sep = "\n")
  cat("Number of missing station coordinates: ", sep = "")
  cat(sum(is.na(x$stnall$longitudestart) | is.na(x$stnall$latitudestart)))
  cat(NULL, sep = "\n")
  cat("Unique species: ", sep = "")
  cat(sort(unique(x$stnall$commonname)), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")

}
