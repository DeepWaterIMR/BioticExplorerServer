#' @title Prepare cruise series list
#' @description Prepares cruise series list using the IMR database API. Stored as data object in the package. Refreshes any new cruise codes. The function does not use any arguments. 
#' @author Ibrahim Umar, Mikko Vihtakari
#' @export

prepareCruiseSeriesList <- function() {
  
  pb <- utils::txtProgressBar(max = 32, style = 3)
  utils::setTxtProgressBar(pb, 1)
  
  # Read cruise reference
  doc <- xml2::read_xml("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/cruiseseries?version=2.0")
  first <- lapply(xml2::xml_find_all(doc, "//d1:row"), function(x) {
    ch <- xml2::xml_children(x)
    y <- xml2::xml_text(ch)
    z <- xml2::xml_name(ch)
    names(y) <- z
    return(as.list(y))
  })
  
  utils::setTxtProgressBar(pb, 2)
  
  # Get per-cruise details
  second <- lapply(seq_along(first), function(i) {
    message(i)
    utils::setTxtProgressBar(pb, i + 2)
    
    x <- first[[i]]
    
    seriesCode <- x[["code"]]
    subdoc <- xml2::read_xml(paste0("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2//model/cruiseseries/", seriesCode, "/samples?version=2.0"))
    years <- xml2::xml_text(xml2::xml_find_all(subdoc, "//d1:sampleTime"))
    
    # Get per-year details
    cruises <- lapply(years, function(y) {
      message(y)
      subsubdoc <- xml2::read_xml(paste0("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2//model/cruiseseries/", seriesCode, "/samples/", y, "/cruises?version=2.0"))
      third <- lapply(xml2::xml_find_all(subsubdoc, "//d1:row"), function(z) {ch <- xml2::xml_children(z); zz <- xml2::xml_text(ch); zzz <- xml2::xml_name(ch); names(zz) <- zzz; return(as.list(zz))})
      subret <- data.table::rbindlist(third, fill = TRUE)
      if(nrow(subret) > 0) subret[, `:=`(code = NULL, year = y, cruiseseriescode = seriesCode)]
      return(subret)
    })
    
    return(data.table::rbindlist(cruises, fill = TRUE))
  })
  
  # Bind them all
  
  utils::setTxtProgressBar(pb, 30)
  
  cruiseSeries <- data.table::rbindlist(second, fill = TRUE)
  cruiseSeries[, description := NULL]
  
  utils::setTxtProgressBar(pb, 31)
  
  # Add the cruise series names
  ref <- data.table::rbindlist(first, fill = TRUE)
  ref[, `:=`(description = NULL, shortname = NULL)]
  cruiseSeries <- merge(cruiseSeries, ref, by.x = "cruiseseriescode", by.y = "code")
  cruiseSeries[, `:=`(cruiseseriescode = as.numeric(cruiseseriescode), year = as.numeric(year))]
  
  utils::setTxtProgressBar(pb, 32)
  
  # Rename columns valid SQL ones
  
  data.table::setnames(cruiseSeries, c("cruisenr", "shipName", "year"), c("cruise", "platformname", "startyear"))
  
  return(cruiseSeries)
}
