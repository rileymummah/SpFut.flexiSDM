#' Make spatial blocks for cross validation
#'
#' @param region (list) output from make_region()
#' @param rows (integer) number of rows in blocks
#' @param cols (integer) number of cols in blocks
#' @param k (integer) number of folds to group blocks into
#'
#' @returns sf object containing blocks
#' @export
#'
#' @examples




make_CV_blocks <- function(region,
                           rows,
                           cols,
                           k) {
  
  blocks <- sf::st_make_grid(region$sp.grid, n = c(cols, rows), square = T, what = "polygons") %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:nrow(.))
  
  # assign folds
  blocks$folds <- rep(1:k, length = nrow(blocks))
  
  # only keep blocks that intersect with region
  blocks1 <- unlist(sf::st_intersects(region$region, blocks))
  blocks <- blocks[blocks1,]
  
  
  blocks <- blocks %>%
    dplyr::mutate(block_id = 1:nrow(blocks)) %>%
    dplyr::rename(geometry = x) %>%
    dplyr::select(id, geometry, block_id, folds)
  
  return(blocks)
  
}

