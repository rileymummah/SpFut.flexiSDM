#' Add state indicator variables
#'
#' @param species.data (list) output from load_species_data()
#' @param region (list) output from make_region()
#' @param gridkey (data.frame) links conus.grid.id and grid.id
#' @param constants (list) output from data_for_nimble()
#' @param covs.inat (character vector) vector of column names (from covar) to use for effort covariates for iNat data
#' @param obsc.state (character vector) abbreviations of states where species has taxon geoprivacy
#' @param keep.conus.grid.id (character vector) vector of conus.grid.ids to keep; default comes from gridkey
#'
#' @returns (list) list of species data partially formatted for nimble, needs to be input into data_for_nimble before use in nimble
#' @export
#'
#' @importFrom tidyselect any_of all_of
#' @importFrom tibble add_column
#' @importFrom readr read_rds
#' @importFrom lubridate yday
#'
#' @examples


add_state_ind <- function(species.data,
                          region,
                          gridkey,
                          constants,
                          covs.inat = NA,
                          obsc.state = NA,
                          keep.conus.grid.id = gridkey$conus.grid.id[which(gridkey$group == "train")]) {
  
  data("grid_states")
  
  # Add state indicator variable for iNat data to indicate which states have taxon geoprivacy
  if ("iNaturalist" %in% names(species.data$obs) & "n.inat" %in% covs.inat) {
    if (length(obsc.state) > 0) {
      grid.states <- stategrid %>%
        dplyr::filter(name %in% obsc.state,
               conus.grid.id %in% keep.conus.grid.id)
      
      # iNat is always dataset1 if it exists
      S1 <- data.frame(grid.id = constants$Wcells1) %>%
        dplyr::left_join(gridkey, by = "grid.id") %>%
        dplyr::mutate(S1 = case_when(conus.grid.id %in% grid.states$conus.grid.id ~ 0,
                              T ~ 1)) %>%
        dplyr::pull(S1)
    } else {
      S1 <- rep(1, nrow(region$sp.grid))
    }
    constants$S1 <- S1
  }
  
  # Add state indicator for multi-state PO to indicate which states have data
  st <- grep("states", names(constants))
  if (length(st) > 0) {
    num <- gsub("states", "", names(constants)[st])
    states <- constants[[st]]
    
    grid.states <- stategrid %>%
      dplyr::filter(name %in% states,
             conus.grid.id %in% keep.conus.grid.id)
    
    S <- data.frame(grid.id = constants[[paste0("Wcells", num)]]) %>%
      dplyr::left_join(gridkey, by = "grid.id") %>%
      dplyr::mutate(S = case_when(conus.grid.id %in% grid.states$conus.grid.id ~ 1,
                           T ~ 0)) %>%
      dplyr::pull(S)
    constants[[paste0("S", num)]] <- S
  }
  
  return(constants)
}

