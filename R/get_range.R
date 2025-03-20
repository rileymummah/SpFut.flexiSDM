#' Get GAP and IUCN species ranges for reference
#'
#' @description
#'
#' @param range.path A vector of paths leading to species ranges
#' @param range.name A vector of names for each range
#' @param crs EPSG code for desired crs of output 
#'
#' @returns A list containing ranges as sf objects
#' @export
#'
#' @examples
#' \dontrun{
#' range <- get_range('ANMI')
#' }


get_range <- function(range.path,
                      range.name = paste0("range", 1:length(range.path)),
                      crs) {
  
  ranges <- list()
  for (p in 1:length(range.path)) {
    
    if(dir.exists(range.path[p]) == F) {
      cat("Path '", range.path[p], "' does not exist\n")
      next
    }
    
    range <- sf::st_read(range.path[p]) %>%
      st_transform(crs = crs)
    
    ranges[[range.name[[p]]]] <- range
  }
  
  return(ranges)
  
}


# 
# 
# 
# get_range <- function(sp.code) {
# 
#   range <- list()
# 
#   if("GAP" %in% list.files(paste0("data/species/", sp.code))) {
#     path1 <- paste0("data/species/", sp.code, "/GAP/")
#     gap.file <- paste0(path1, list.files(path1)[1])
#     gap.layer <- list.files(path1)[1]
#     gap <- sf::st_read(dsn = gap.file, layer = gap.layer)
# 
#     range[["GAP"]] <- gap
#   } else {
#     cat("GAP map has not been downloaded yet\n")
#   }
# 
#   if("IUCN" %in% list.files(paste0("data/species/", sp.code))) {
#     path1 <- paste0("data/species/", sp.code, "/IUCN/")
#     iucn <- sf::st_read(dsn = path1, layer = "data_0")
# 
#     range[["IUCN"]] <- iucn
# 
#   } else {
#     cat("IUCN map has not been downloaded yet\n")
#   }
# 
#   return(range)
# 
# }
