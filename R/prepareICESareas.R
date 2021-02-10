#' @title Download and format ICES area polygons
#' @details This function is an Ibrahim hack which does not work optimally because the area polygons supplied on the ICES site are very complex and partly corrupted. Use the premade polygons (\code{data(icesAreas)}) instead. These polygons have been obtained from the ICES GIS team upon request and much simpler. 
#' @importFrom utils unzip
#' @importFrom rgdal readOGR
#' @author Ibrahim Umar
#' @keywords internal
prepareICESareas <- function() {
    dst <- tempfile()
    download.file("https://gis.ices.dk/shapefiles/ICES_areas.zip", dst)
    shpfiles <- utils::unzip(dst, exdir = paste0(dirname(dst), "/shape"))
    poly <- rgdal::readOGR(dirname(shpfiles[1]))
    unlink(dirname(shpfiles[1]), recursive = TRUE)
    unlink(dst)
    return(poly)
}

