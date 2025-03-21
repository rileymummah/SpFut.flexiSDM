#' Get species ranges for reference
#'
#' @description Assembles pre-downloaded species range shapefiles (e.g., from IUCN or GAP)
#'
#' @param range.path (character vector) A vector of paths leading to species ranges
#' @param range.name (character vector) A vector of names for each range
#' @param crs (numeric) EPSG code for desired coordinate reference system (CRS) of output
#'
#' @returns A list containing ranges as sf objects
#' @export
#'
#' @importFrom tidyselect everything
#'
#' @examples
#'\dontrun{
#' # Predownload range boundaries of interest
#'
#' # Provide the path to each boundary
#' range.path <- c("GAP/aAZTOx_CONUS_Range_2001v1/",
#'                 "IUCN/")
#'
#' # Give each boundary a name
#' range.name <- c("GAP", "IUCN")
#'
#' rangelist <- get_range(range.path,
#'                        range.name,
#'                        crs = 4326)
#'}



get_range <- function(range.path,
                      range.name = paste0("range", 1:length(range.path)),
                      crs) {

  if (length(range.name) != length(range.path)) stop(cat("range.path and range.name must have same length"))

  ranges <- list()
  for (p in 1:length(range.path)) {

    if(dir.exists(range.path[p]) == F) {
      cat("Path '", range.path[p], "' does not exist\n")
      next
    }

    range <- sf::st_read(range.path[p]) %>%
              sf::st_transform(crs = crs) %>%
              dplyr::mutate(range.name = range.name[p]) %>%
              dplyr::select(range.name, tidyselect::everything())

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
