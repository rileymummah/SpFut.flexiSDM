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
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate filter rename filter pull summarize group_by


select_covar <- function(covs,
                         threshold = 0.4) {

  # selectively remove covariates until all high correlations are gone
  # cors <- cor(as.data.frame(.data$covar[,c(covs)])) %>%
  cors <- as.data.frame(cor(covs))
  cors$cov1 <- rownames(cors)
  cors <- cors %>%
            # mutate(cov1 = row.names()) %>%
            pivot_longer(!"cov1") %>%
            filter(.data$value != 1) %>%
            rename(cov2 = "name")


  # get the max correlation
  rm.vals <- max(abs(cors$value))

  covs.rm <- c()
  while (rm.vals > threshold) {
    rm.var <- cors$cov1[which(abs(cors$value) %in% rm.vals)]

    tmp <- cors %>%
            filter(.data$cov1 %in% rm.var) %>%
            group_by(.data$cov1) %>%
            summarize(avg = mean(abs(.data$value))) %>%
            filter(.data$avg == max(.data$avg)) %>%
            pull(.data$cov1)

    covs.rm <- c(covs.rm, tmp)

    cors <- filter(cors, .data$cov1 != tmp, .data$cov2 != tmp)

    # get the new max correlation
    rm.vals <- max(abs(cors$value))
  }

  return(covs.rm)
}
