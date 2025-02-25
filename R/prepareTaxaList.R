#' @title Prepare taxa list
#' @description Prepares taxa list using the IMR database API. Stored as data object in the package. Refreshes any new taxa codes. 
#' @param verbose Logical indicating whether the function should return status information as messages.
#' @author Mikko Vihtakari, Ibrahim Umar
#' @export

prepareTaxaList <- function(verbose = TRUE) {
  
  # Read gear list reference
  if(verbose) message("Downloading https://reference-api.hi.no/apis/nmdapi/reference/v2/dataset/taxa?version=2.0...")
  
  doc <- xml2::read_xml("https://reference-api.hi.no/apis/nmdapi/reference/v2/dataset/taxa?version=2.0")
  
  if(verbose) message("Merging tsn lists...")
  
  tmp.in <- xml2::xml_find_all(doc, "//d1:row")
  
  # i = 5344
  tmp <- lapply(seq_along(tmp.in), function(i) {
    # print(i)
    ch <- xml2::xml_children(tmp.in[[i]])
    
    y <- data.frame(t(xml2::xml_text(ch)))
    names(y) <- xml2::xml_name(ch)
    y[names(y) %in% c("tsn","aphiaid","nodc", "pgnapes")] 
  })
    
  out <- data.table::rbindlist(tmp, fill = TRUE)
  
  ## Add species names
  
  if(verbose) message("Merging species names...")
  
  tmp2.in <- xml2::xml_find_all(doc, "//d1:TaxaSynonyms")
  
  # i = 5344
  tmp <- lapply(seq_along(tmp2.in), function(i) {
    # print(i)
    ch <- xml2::xml_children(tmp2.in[[i]])
    
    y <- data.frame(t(xml2::xml_text(xml2::xml_children(ch))))
    names(y) <- xml2::xml_name(xml2::xml_children(ch))
    
    z <- y[names(y) %in% "name"]
    names(z) <- make.names(tolower(as.character(y[names(y) %in% "language"])), 
                           unique = TRUE)
    
    z
  })
  
  out2 <- data.table::rbindlist(tmp, fill = TRUE)
  
  ## Combine and format
  
  if(verbose) message("Finishing...")
  
  if(nrow(out) != nrow(out2)) stop("The number of tsn and species name rows does not match. Debug.")
  
  out <- cbind(out, out2)
  
  splist <- data.table::melt(out, id = c("tsn", "aphiaid", "nodc", "pgnapes"),
                   variable.name = "language", value.name = "name", 
                   na.rm = TRUE)
  
  splist[,language := factor(gsub("\\..$", "", as.character(splist$language)))]
  
  return(splist)
}
