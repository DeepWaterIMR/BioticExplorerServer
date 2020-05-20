#' @title Read and process a NMD Biotic xml file for further use in the BioticExplorer database
#' @description A wrapper for \code{\link[RstoxData]{readXmlFile}} to enable further use in the BioticExplorer database
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param removeEmpty logical indicating whether empty columns should be removed from the output. This option also influences "coreData" columns.
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function, but leads to problems with non-unicode characters.
#' @param missionidPrefix A prefix for the \code{missionid} identifier, which separates cruises. \code{NULL} (default) omits the prefix. Use year when writing to the database.
#' @param icesAreaShape ICES area shape in SpatialPolygonsDataFrame object. Used for calculating the ICES area for a specific fishstation.
#' @param cruiseSeries a data.table object of NMD cruise series list. Used to identify cruise series of a specific mission.
#' @return Returns a list of Biotic data with the \code{$mission} data table from the original NMD data. The \code{$stnall} and \code{$indall} data frames are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter).
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @import RstoxData data.table
#' @importFrom sp over coordinates proj4string CRS spTransform
#' @export

# Debugging parameters
bioticToDatabase <- function(file, removeEmpty = FALSE, convertColumns = TRUE, missionidPrefix = NULL, icesAreaShape = NULL, cruiseSeries) {

  ## Checks

  if(!file.exists(file)) stop("file does not exist. Check your file path.")

  ## Read the Biotic file ----

  dt <- RstoxData::readXmlFile(file)

  ## Mission data ---

  msn <- dt$mission

  # Add cruise series information
  msn <- merge(msn, cruiseSeries, by.x = c("startyear", "platformname"), by.y = c("year", "shipName"))

  if (convertColumns) {
    date.cols <- grep("date", names(msn), value = TRUE)
    msn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }

  if (is.null(missionidPrefix)) {
    msn$missionid <- rownames(msn)
  } else {
    msn$missionid <- paste(missionidPrefix, rownames(msn), sep = "_")
  }

  ## Station data ---

  stn <- dt$fishstation

  stn[is.na(stationstarttime), stationstarttime := "00:00:00.000Z"]

  stn[, stationstartdate := as.POSIXct(paste(stn$stationstartdate, stn$stationstarttime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]

  stn[, stationstarttime := NULL]

  ### Add ICES area
  if(!is.null(icesAreaShape)) {
    points <- stn[, c("longitudestart", "latitudestart")]

    # Remove NAs (set longlat as 0 so that translation gives NA)
    points[is.na(longitudestart) | is.na(latitudestart), `:=`(longitudestart=0, latitudestart = 0)]

    coordinates(points) <- c(1,2)
    proj4string(points) <- CRS("+init=epsg:4326")

    transformedPoints <- spTransform(points, proj4string(icesAreaShape))

    stn[, icesarea:= over(transformedPoints, icesAreaShape)$Area_Full]
  }

  if (convertColumns) {
    date.cols <- grep("date", names(stn), value = TRUE)
    stn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }

  ##________________
  ## Sample data ---

  cth <- dt$catchsample

  ##____________________
  ## Individual data ---

  ind <- dt$individual

  ## Age data ---

  age <- dt$agedetermination

  if (convertColumns) {
    date.cols <- grep("date", names(age), value = TRUE)
    age[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }

  # if (nrow(age) == 0) {
  #   age <- rapply(age, as.integer, how = "replace")
  # }

  ## Compiled datasets ----

  coredat <- merge(msn[,!names(msn) %in% c("purpose")], stn, by = names(msn)[names(msn) %in% names(stn)], all = TRUE)

  # Stndat

  stndat <- merge(coredat, cth, all.y = TRUE, by = c("missiontype", "missionnumber", "startyear", "platform", "serialnumber"))

  # Inddat

  inddat <- merge(stndat[,!names(stndat) %in% c("purpose", "stationcomment"), with = FALSE], ind, all.y = TRUE, by = names(stndat)[names(stndat) %in% names(ind)])

  inddat[is.na(preferredagereading), preferredagereading := 1]
  inddat <- merge(inddat, age, by.x=c(intersect(names(inddat), names(age)), "preferredagereading"), by.y= c(intersect(names(inddat), names(age)), "agedeterminationid"), all.x = TRUE)

  # inddat[is.na(inddat$commonname), "commonname"] <- "Merging error due to missing data"

  ## Return ----

  ### Format

  out <- list(mission = msn, stnall = stndat, indall = inddat)

  ### Remove empty columns to save space

  if (removeEmpty) {
    out <- lapply(out, function(k) {
      k[, which(unlist(lapply(k, function(x) !all(is.na(x))))), with = FALSE]
    })
  }

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
