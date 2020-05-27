#' @title Prepare gear list
#' @description Prepares gear list using the IMR database API. Stored as data object in the package. Refreshes any new gear codes. The function does not use any arguments. 
#' @importFrom xml2 read_xml xml_find_all xml_children xml_text xml_name
#' @importFrom data.table rbindlist := setnames setcolorder
#' @author Mikko Vihtakari, Ibrahim Umar
#' @export

prepareGearList <- function() {
  
  # Read gear list reference
  doc <- xml2::read_xml("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/equipment?version=2.0")
  
  tmp <- lapply(xml2::xml_find_all(doc, "//d1:row"), function(x) {
    ch <- xml2::xml_children(x)
    y <- xml2::xml_text(ch)
    z <- xml2::xml_name(ch)
    names(y) <- z
    return(data.table(t(y)))
  })
  
  # Bind to data table
  gearList <- data.table::rbindlist(tmp, fill = TRUE)
  
  # Gear categories
  tmp <- ifelse(nchar(gearList$code) != 4, NA, gearList$code)
  tmp <- substr(tmp, 1,2)
  tmp[tmp %in% 10:11] <- "Water samplers"
  tmp[tmp %in% 20:21] <- "Plankton nets"
  tmp[tmp %in% 22] <- "Pumps"
  tmp[tmp %in% 23] <- "Tow nets"
  tmp[tmp %in% c(24, 44)] <- "Scrapes"
  tmp[tmp %in% 25] <- "Grabs"
  tmp[tmp %in% 30:33] <- "Bottom trawls"
  tmp[tmp %in% 34] <- "Other trawls"
  tmp[tmp %in% 35] <- "Pelagic trawls"
  tmp[tmp %in% 36:37] <- "Seines"
  tmp[tmp %in% 40:41] <- "Gillnets"
  tmp[tmp %in% c(42, 43, 53)] <- "Traps"
  tmp[tmp %in% 50:52] <- "Hooks"
  tmp[tmp %in% 60:90] <- "Other"
  tmp[is.na(tmp)] <- "Other"
  
  gearList[, gearcategory := tmp]
  
  # Column order and clean-up
  
  gearList[, area := NULL]
  data.table::setnames(gearList, "name", "gearname")
  data.table::setcolorder(gearList, c("code", "gearname", "gearcategory", "description"))
  
  # Return
  return(gearList)
}
