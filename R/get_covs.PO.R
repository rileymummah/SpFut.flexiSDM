get_covs.PO <- function(covs.PO,
                        covar) {
  if ("n.inat" %in% covs.PO & "n.inat" %in% colnames(covar)) {
    po.cov <- covar %>%
      select(conus.grid.id, all_of(covs.PO))

  } else if ("n.inat" %in% covs.PO & "n.inat" %in% colnames(covar) == F){
    covs.PO1 <- covs.PO[-grep("n.inat", covs.PO)]
    po.cov <- covar %>%
      select(conus.grid.id, all_of(covs.PO1))

  } else if ("n.inat" %in% covs.PO == F) {
    po.cov <- covar %>%
      select(conus.grid.id, all_of(covs.PO))
  }

  return(po.cov)
}
