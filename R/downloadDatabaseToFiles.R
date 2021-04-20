#' @title Download and parse NMD data for the BioticExplorer database
#' @description Downloads annual NMD data from the API and writes them as MonetDB database
#' @param years Vector of integer specifying the years to be downloaded. The database reaches 1914:2020
#' @param dest Character string specifying the file path where the database should be downloaded to. 
#' @param method,save Switches that do not make much sense at the moment. Leave them as they are.
#' @param icesAreaShape ICES area shape in SpatialPolygonsDataFrame object. Used for calculating the ICES area for a specific fishstation.
#' @param cruiseSeries a data.table object of NMD cruise series list. Used to identify cruise series of a specific mission. See \code{\link{prepareCruiseSeriesList}}.
#' @param gearCodes a data.table object of NMD gear code list. Used to make gearname and gearcategory columns. See \code{\link{prepareGearList}}.
#' @details This function is scarily powerful. Do not run a large number of years unless you think you know what you are doing
#' @import data.table DBI MonetDBLite
#' @importFrom utils download.file
#' @author Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
#' @export

# dest <- "~/Desktop/IMR database"
# icesAreaShape = icesAreas; cruiseSeries = cruiseSeriesList; gearCodes = gearList
# method = "compare", "keep", "update"
# save = "xml", "rds", 
downloadDatabaseToFiles <- function(years, dest, method = "compare", save = c("xml", "rds"), icesAreaShape = icesAreas, cruiseSeries = cruiseSeriesList, gearCodes = gearList) {

  if(!dir.exists(dest)) {
    
    msg <- paste0("dest is pointing to ", dest, ", which does not exist.", " Do you want to create the directory?")
    
    message(paste(strwrap(msg), collapse= "\n"))
    ret.val <- utils::menu(c("Yes", "No"), "")
    
    if(ret.val != 1) {
      msg <- paste0(dest, " does not exist. Cannot download data.")
      stop(paste(strwrap(msg), collapse= "\n"))
    } else {
      dir.create(dest)
      dir.create(file.path(dest, "XMLfiles"))
      dir.create(file.path(dest, "Rdata"))
      msg <- paste0("The data download directory created to ", dest)
      message(paste(strwrap(msg), collapse= "\n"))
    }
  }
  
  # h <- years[[1]]
  lapply(years, function(h) {
    message(paste("Processing:", h))

    fileFound <- file.exists(paste0(file.path(dest, "XMLfiles"), "/", h, ".xml"))
    
    if(method == "keep" & fileFound) {
      return("xml file already found. Method = keep. Skipping...")
    }
    
    url <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/", h, "/cache?version=3.1")
    status <- suppressMessages(suppressWarnings(try(utils::download.file(url, paste0(file.path(dest, "XMLfiles"), "/", h, ".xml"), silent = TRUE))))

    if(class(status) == "try-error") {
      if(!is.na(file.info(paste0(file.path(dest, "XMLfiles"), "/", h, ".xml"))$size)) stop("Download timeout error. Current timeout ", getOption('timeout'),". Set a higher timeout limit using options(timeout = ...)")
      message(paste("Year", h, "not found from the database. Skipping..."))
    } else {
      
      message("xml saved")
      
      # Do transformations

      a <- bioticToDatabase(paste0(file.path(dest, "XMLfiles"), "/", h, ".xml"), missionidPrefix = h, icesAreaShape = icesAreaShape, cruiseSeries = cruiseSeriesList, gearCodes = gearCodes)
      
      if("rds" %in% save) {
        saveRDS(a, file = paste0(file.path(dest, "Rdata"), "/", h, ".rds"), compress = "xz")
        message("rds saved")
      }
      
      return(message(paste("Year", h, "is parsed successfully.")))
    }
  })
  
}
