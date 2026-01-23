#' Get state intersections with grid
#'
#' @param region (list) output from make_region()
#' @param states (data.frame) links conus.grid.id and grid.id
#'
#' @returns (data.frame) list of conus.grid.ids and which states they intersect with; can have multiple rows for one conus.grid.id if it overlaps multiple states
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom dplyr filter left_join mutate pull case_when

get_state_grid <- function(region,
                           states) {
  
  states <- st_transform(states, st_crs(region$sp.grid))
  suppressWarnings(stategrid <- st_intersection(region$sp.grid, states) %>%
                     st_drop_geometry() %>%
                     mutate(value = 1,
                            name = postal,
                            conus.grid.id = as.character(conus.grid.id)) %>%
                     select("conus.grid.id", "name", "value"))
  
  return(stategrid)
}