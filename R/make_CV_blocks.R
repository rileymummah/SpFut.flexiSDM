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
#' @importFrom rlang .data
#' @importFrom sf st_make_grid st_as_sf st_intersects
#' @importFrom dplyr mutate rename select n
#' @importFrom magrittr "%>%"


make_CV_blocks <- function(region,
                           rows,
                           cols,
                           k) {

  blocks <- st_make_grid(region$sp.grid, n = c(cols, rows),
                         square = T, what = "polygons") %>%
    st_as_sf() %>%
    mutate(id = 1:n())

  # assign folds
  blocks$folds <- rep(1:k, length = nrow(blocks))

  # only keep blocks that intersect with region
  blocks1 <- unlist(st_intersects(region$region, blocks))
  blocks <- blocks[blocks1,]


  blocks <- blocks %>%
    mutate(block_id = 1:nrow(blocks)) %>%
    rename(geometry = "x") %>%
    select("id", "geometry", "block_id", "folds")

  return(blocks)

}

