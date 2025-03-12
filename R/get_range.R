#' Get GAP and IUCN species ranges for reference
#'
#' @description
#'
#' @param sp.code
#'
#' @returns A list containing the GAP and IUCN species ranges (if pre-downloaded and available)
#' @export
#'
#' @examples
#' \dontrun{
#' range <- get_range('ANMI')
#' }


get_range <- function(sp.code) {

  range <- list()

  if("GAP" %in% list.files(paste0("data/species/", sp.code))) {
    path1 <- paste0("data/species/", sp.code, "/GAP/")
    gap.file <- paste0(path1, list.files(path1)[1])
    gap.layer <- list.files(path1)[1]
    gap <- sf::st_read(dsn = gap.file, layer = gap.layer)

    range[["GAP"]] <- gap
  } else {
    cat("GAP map has not been downloaded yet\n")
  }

  if("IUCN" %in% list.files(paste0("data/species/", sp.code))) {
    path1 <- paste0("data/species/", sp.code, "/IUCN/")
    iucn <- sf::st_read(dsn = path1, layer = "data_0")

    range[["IUCN"]] <- iucn

  } else {
    cat("IUCN map has not been downloaded yet\n")
  }

  return(range)

}
