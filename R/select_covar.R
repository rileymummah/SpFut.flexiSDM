#' Select covariates that have low correlations
#'
#' @description
#' Selectively remove covariates from a pool of covariates until all high correlations are gone.
#'
#'
#' @param covs (data.frame) dataframe holding pool of covariates to choose from
#' @param threshold (numeric) threshold for correlations above which covariates should be removed; defaults to 0.4
#'
#' @returns (character vector) vector of covariate names to remove
#' @export
#'
#' @importFrom tidyr pivot_longer
#'
#' @examples




select_covar <- function(covs,
                         threshold = 0.4) {

  # selectively remove covariates until all high correlations are gone
  cors <- cor(as.data.frame(covar[,c(covs)])) %>%
    as.data.frame() %>%
    dplyr::mutate(cov1 = row.names(.)) %>%
    tidyr::pivot_longer(!cov1) %>%
    dplyr::filter(value != 1) %>%
    dplyr::rename(cov2 = name)


  # get the max correlation
  rm.vals <- max(abs(cors$value))

  covs.rm <- c()
  while (rm.vals > threshold) {
    rm.var <- cors$cov1[which(abs(cors$value) %in% rm.vals)]

    tmp <- cors %>%
            dplyr::filter(cov1 %in% rm.var) %>%
            dplyr::group_by(cov1) %>%
            dplyr::summarize(avg = mean(abs(value))) %>%
            dplyr::filter(avg == max(avg)) %>%
            dplyr::pull(cov1)

    covs.rm <- c(covs.rm, tmp)
    #cat("removing ", tmp, "\n")

    cors <- dplyr::filter(cores,
                          cov1 != tmp,
                          cov2 != tmp)

    # get the new max correlation
    rm.vals <- max(abs(cors$value))
  }

  return(covs.rm)
}
