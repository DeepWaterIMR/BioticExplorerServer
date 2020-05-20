# Define global variables
utils::globalVariables(c("stationstarttime", "stationstartdate", "preferredagereading", "commonname", "cruise", "gear",
"missionid", "missiontypename", "platformname", "serialnumber", "startyear", "icesarea", "coordinates<-", "proj4string<-", 
"longitudestart", "latitudestart", "description", "cruiseseriescode"))

#' @importFrom utils unzip
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

#' @importFrom xml2 read_xml xml_find_all xml_children xml_text xml_name
#' @importFrom data.table rbindlist :=
prepareCruiseSeriesList <- function() {
    # Read cruise reference
    doc <- read_xml("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2//model/cruiseseries?version=2.0")
    first <- lapply(xml_find_all(doc, "//d1:row"), function(x) {ch <- xml_children(x); y <- xml_text(ch); z <- xml_name(ch); names(y) <- z; return(as.list(y))})

    # Get per-cruise details
    second <- lapply(first, function(x) {
        seriesCode <- x[["code"]]
        subdoc <- read_xml(paste0("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2//model/cruiseseries/", seriesCode, "/samples?version=2.0"))
        years <- xml_text(xml_find_all(subdoc, "//d1:sampleTime"))

        # Get per-year details
        cruises <- lapply(years, function(y) {
            subsubdoc <- read_xml(paste0("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2//model/cruiseseries/", seriesCode, "/samples/", y, "/cruises?version=2.0"))
            third <- lapply(xml_find_all(subsubdoc, "//d1:row"), function(z) {ch <- xml_children(z); zz <- xml_text(ch); zzz <- xml_name(ch); names(zz) <- zzz; return(as.list(zz))})
            subret <- rbindlist(third, fill = TRUE)
            subret[, `:=`(code = NULL, year = y, cruiseseriescode = seriesCode)]
            return(subret)
        })
        return(rbindlist(cruises, fill = TRUE))
    })

    # Bind them all
    cruiseSeries <- rbindlist(second, fill = TRUE)
    cruiseSeries[, description := NULL]

    # Add the cruise series names
    ref <- rbindlist(first, fill = TRUE)
    ref[, `:=`(description = NULL, shortname = NULL)]
    cruiseSeries <- merge(cruiseSeries, ref, by.x = "cruiseseriescode", by.y = "code")
    cruiseSeries[, `:=`(cruiseseriescode = as.numeric(cruiseseriescode), year = as.numeric(year))]
    return(cruiseSeries)
}