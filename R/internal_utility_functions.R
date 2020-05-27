# Define global variables
utils::globalVariables(c("stationstarttime", "stationstartdate", "preferredagereading", "commonname", "cruise", "gear",
"missionid", "missiontypename", "platformname", "serialnumber", "startyear", "icesarea", "stationstopdate", "stationstoptime",
"longitudestart", "latitudestart", "description", "cruiseseriescode", "timestart", "timeend", "..ints", "name", "cruiseSeriesList", 
"cruise_series", "ICESareas", "gearcategory", "area", "gearList"))

#' @importFrom utils unzip
#' @importFrom rgdal readOGR
prepareICESareas <- function() {
    dst <- tempfile()
    download.file("https://gis.ices.dk/shapefiles/ICES_areas.zip", dst)
    shpfiles <- utils::unzip(dst, exdir = paste0(dirname(dst), "/shape"))
    poly <- rgdal::readOGR(dirname(shpfiles[1]))
    unlink(dirname(shpfiles[1]), recursive = TRUE)
    unlink(dst)
    return(poly)
}

