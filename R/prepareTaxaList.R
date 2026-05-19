#' @title Prepare taxa list
#' @description Downloads and compiles the taxa reference list from the IMR API. The result can be bundled as package data. Run this function and save with \code{usethis::use_data(taxaList, overwrite = TRUE)} to refresh the bundled data.
#' @param verbose Logical indicating whether the function should print progress messages.
#' @return A \code{\link[data.table]{data.table}} with columns \code{tsn} and \code{name} (plus additional synonym columns where available). Rows where \code{name} is \code{NA} are excluded.
#' @author Mikko Vihtakari, Ibrahim Umar
#' @export

prepareTaxaList <- function(verbose = FALSE) {
  
  # Read gear list reference
  if(verbose) message("Downloading https://reference-api.hi.no/apis/nmdapi/reference/v2/dataset/taxa?version=2.0...")
  
  doc <- xml2::read_xml("https://reference-api.hi.no/apis/nmdapi/reference/v2/dataset/taxa?version=2.0")
  
  if(verbose) message("Reading the xml file...")
  
  x <- xml2::as_list(doc)
  
  if(verbose) message("Merging tsn lists...")
  
  ## ####
  
  out <- lapply(seq_along(x$list), function(i) {
    # message(i)
    if(!is.null(x$list[[i]]$TaxaSynonyms)) {
      
      tmp <- lapply(x$list[[i]][[c("TaxaSynonyms")]], function(k){ 
        data.frame(t(unlist(k)))
        })
      
      cbind(
        data.frame(t(unlist(x$list[[i]][c("tsn")]))),
        data.table::rbindlist(tmp, fill = TRUE)
      )
    } else {
      data.frame(t(unlist(x$list[[i]][c("tsn")])))
    }
  })
  
  ## Combine and format ####
  
  if(verbose) message("Finishing...")
  
  splist <- data.table::rbindlist(out, fill = TRUE)
  
  return(splist[!is.na(name),])
}
