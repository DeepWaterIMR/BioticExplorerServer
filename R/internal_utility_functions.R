# Define global variables
utils::globalVariables(c("stationstarttime", "stationstartdate", "preferredagereading", "commonname", "cruise", "gear",
"missionid", "missiontypename", "platformname", "serialnumber", "startyear", "icesarea", "coordinates<-", "proj4string<-", "longitudestart", "latitudestart"))

#' @importFrom  utils unzip
#' @importFrom rgdal readOGR
prepareICESareas <- function() {
    dst <- tempfile()
    download.file("https://gis.ices.dk/shapefiles/ICES_areas.zip", dst)
    shpfiles <- unzip(dst, exdir = paste0(dirname(dst), "/shape"))
    poly <- readOGR(dirname(shpfiles[1]))
    unlink(dirname(shpfiles[1]), recursive = TRUE)
    unlink(dst)
    return(poly)
}