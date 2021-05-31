#' @title Cruise series list
#' @description Use the \code{\link{prepareCruiseSeriesList}} function to update the list. Need to be updated every time data from the database are downloaded.
#' @docType data
#' @keywords datasets
#' @name cruiseSeries
#' @format A data table 
#' @source Institute of Marine Research (\url{https://imr.no})
"cruiseSeries"

#' @title Gear code list
#' @description Use the \code{\link{prepareGearList}} function to update the list.
#' @docType data
#' @keywords datasets
#' @name gearList
#' @format A data table 
#' @source Institute of Marine Research (\url{https://imr.no})
"gearList"

#' @title ICES fishing areas
#' @description Food and Agriculture Organization Major Fishing Area 27 (i.e. ICES region) fishing area polygons. The polygons are not cut with land as opposed to the areas distributed on the ICES website (see \code{BioticExplorerServer:::prepareICESareas}). This makes the polygons smaller and consequent calculations quicker. Use this dataset instead of the function.
#' @docType data
#' @keywords datasets shapefiles
#' @family shapefiles
#' @name icesAreas
#' @format \code{\link[sf]{st_polygon}} in decimal degrees (EPSG:4326).
#' @source \href{https://gis.ices.dk/sf/index.html}{International Council for the Exploration of the Sea}
"icesAreas"

