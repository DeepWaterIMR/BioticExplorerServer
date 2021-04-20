.onLoad <- function(libname, pkgname) {
  options("rgdal_show_exportToProj4_warnings"="none")
}

.onAttach <- function(libname, pkgname) {
  options(timeout = max(300, getOption("timeout"))) 
}

# Define global variables
utils::globalVariables(c("stationstarttime", "stationstartdate", "preferredagereading", "commonname", "cruise", "gear", "missionid", "missiontypename", "platformname", "serialnumber", "startyear", "icesarea", "stationstopdate", "stationstoptime",
                         "longitudestart", "latitudestart", "description", "cruiseseriescode", "timestart", "timeend", "..ints", "name", "cruiseSeriesList", 
                         "cruise_series", "ICESareas", "gearcategory", "area", "gearList", "icesAreas", "filesize", "language"))

