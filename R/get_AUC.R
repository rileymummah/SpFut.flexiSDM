#' Get AUC
#'
#' @param species.data (list) output from load_species_data()
#' @param out (list) output from summarize_samples()
#'
#' @returns (data.frame) Overall AUC and AUC for each dataset
#' @export
#'
#'
#' @examples


get_AUC <- function(species.data,
                   out) {
  val.dat <- c()
  for (d in 1:length(species.data$obs)) {
    tmp <- species.data$obs[[d]] %>%
      dplyr::mutate(site.id = as.character(site.id))
    val.dat <- dplyr::bind_rows(val.dat, tmp)
  }
  
  val.dat <- val.dat %>%
    dplyr::filter(data.type != "PO") %>%
    dplyr::mutate(pa = case_when(count > 0 ~ 1,
                          count == 0 ~ 0)) %>%
    dplyr::select(conus.grid.id, source, pa)
  
  # If it's only PO data, skip the following
  if (nrow(val.dat) > 0) {

    # Calculate AUC for in sample
    val.in <- dplyr::filter(out$psi0, group == "train") %>%
      dplyr::inner_join(val.dat, by = "conus.grid.id")
    
    # overall
    if (length(unique(val.in$pa)) < 2) {
      AUCin <- NA
    } else {
      AUCin <- pROC::auc(val.in$pa, val.in$mean)
    }
    
    # each dataset
    dats <- unique(val.in$source)
    aucsin <- c()
    for (s in 1:length(dats)) {
      val.in1 <- dplyr::filter(val.in, source == dats[s])
      
      if (length(unique(val.in1$pa)) < 2) {
        AUCin1 <- NA
      } else {
        AUCin1 <- pROC::auc(val.in1$pa, val.in1$mean)
      }
      
      tmpin <- data.frame(source = dats[s],
                          AUCin = as.numeric(AUCin1),
                          in.n = nrow(val.in1),
                          in.cell = length(unique(val.in1$conus.grid.id)))
      
      aucsin <- dplyr::bind_rows(aucsin, tmpin)
    }
    

    
    # Calculate AUC for out of sample
    val.out <- dplyr::filter(out$psi0, group == "test") %>% dplyr::inner_join(val.dat, by = "conus.grid.id")
    
    # Overall
    if (nrow(val.out) == 0) {
      # if there is no out of sample data
      AUCout <- NA
      
    } else {
      # if there is out of sample data
      if (length(unique(val.out$pa)) < 2) {
        AUCout <- NA
      } else {
        AUCout <- pROC::auc(val.out$pa, val.out$mean)
      }
    }
    
    
    # each dataset
    dats <- unique(val.out$source)
    aucsout <- c()
    for (s in 1:length(dats)) {
      val.out1 <- filter(val.out, source == dats[s])
      
      if (length(unique(val.out1$pa)) < 2) {
        AUCout1 <- NA
      } else {
        AUCout1 <- pROC::auc(val.out1$pa, val.out1$mean)
      }
      
      tmpout <- data.frame(source = dats[s],
                           AUCout = as.numeric(AUCout1),
                           out.n = nrow(val.out1),
                           out.cell = length(unique(val.out1$conus.grid.id)))
      
      aucsout <- dplyr::bind_rows(aucsout, tmpout)
      
    }
    
    
    
    if (block.out != "none") {aucs <- dplyr::full_join(aucsin, aucsout, by = "source")}
    if (block.out == "none") {aucs <- aucsin}
    
    all.auc <- data.frame(sp.code = sp.code,
                          block = block.out,
                          AUCin.full = as.numeric(AUCin), 
                          in.full.n = nrow(val.in),
                          in.full.cell = length(unique(val.in$conus.grid.id)),
                          AUCout.full = as.numeric(AUCout),
                          out.full.n = nrow(val.out),
                          out.full.cell = length(unique(val.out$conus.grid.id)),
                          aucs)
    
  }
  
  # If there are no validation data, set auc.out = NA
  if (!exists("all.auc")) {
    all.auc <- NA
  }
  
  return(all.auc)
}



