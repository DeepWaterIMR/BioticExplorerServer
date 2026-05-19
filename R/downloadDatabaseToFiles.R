#' @title Download and parse NMD Biotic data to files
#' @description Downloads annual NMD Biotic data from the API and saves them as XML and/or RDS files on disk. Unlike \code{\link{downloadDatabase}}, this function does not write to a DuckDB database.
#' @param dest Character string specifying the folder path where downloaded files should be saved. Subdirectories \code{XMLfiles/} and \code{Rdata/} are created automatically.
#' @param method Character string controlling what to do when a file already exists. \code{"compare"} (default) re-downloads and overwrites; \code{"keep"} skips existing files.
#' @param save Character vector of output formats. Can include \code{"xml"} (raw XML, always saved) and/or \code{"rds"} (parsed \code{\link{bioticToDatabase}} output as RDS).
#' @inheritParams downloadDatabase
#' @return Called for its side effects: saves XML and/or RDS files to \code{dest}. Returns \code{NULL} invisibly.
#' @import data.table
#' @author Ibrahim Umar, Mikko Vihtakari (Institute of Marine Research)
#' @export

# dest <- "~/Desktop/IMR database"
# icesAreaShape = icesAreas; cruiseSeries = cruiseSeriesList; gearCodes = gearList
# method = "compare", "keep", "update"
# save = "xml", "rds", 
downloadDatabaseToFiles <- function(years, dest, method = "compare", save = c("xml", "rds"), icesAreas = NULL, cruiseSeries = NULL, gearCodes = NULL) {

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
    
    url <- paste0("https://biotic-api.hi.no/apis/nmdapi/biotic/v3/", h, "/cache?version=3.1")
    status <- suppressMessages(suppressWarnings(try(utils::download.file(url, paste0(file.path(dest, "XMLfiles"), "/", h, ".xml"), silent = TRUE))))

    if(inherits(status, "try-error")) {
      if(!is.na(file.info(paste0(file.path(dest, "XMLfiles"), "/", h, ".xml"))$size)) stop("Download timeout error. Current timeout ", getOption('timeout'),". Set a higher timeout limit using options(timeout = ...)")
      message(paste("Year", h, "not found from the database. Skipping..."))
    } else {
      
      message("xml saved")
      
      # Do transformations

      a <- bioticToDatabase(paste0(file.path(dest, "XMLfiles"), "/", h, ".xml"), 
                            missionidPrefix = h, icesAreas = icesAreas, cruiseSeries = cruiseSeries, gearCodes = gearCodes)
      
      if("rds" %in% save) {
        saveRDS(a, file = paste0(file.path(dest, "Rdata"), "/", h, ".rds"), compress = "xz")
        message("rds saved")
      }
      
      return(message(paste("Year", h, "is parsed successfully.")))
    }
  })
  
}
