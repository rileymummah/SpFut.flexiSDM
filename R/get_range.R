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
#' @importFrom sf st_read st_transform
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select mutate


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

    range <- st_read(range.path[p]) %>%
              st_transform(crs = crs) %>%
              mutate(range.name = range.name[p]) %>%
              select(range.name, everything())

    ranges[[range.name[[p]]]] <- range
  }

  return(ranges)

}
