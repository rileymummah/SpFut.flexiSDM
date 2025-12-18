#' Summarize a single chain from NIMBLE output
#'
#' @param i (numeric) index of parameter to summarize
#' @param samples (list) output from NIMBLE
#' @param chains (numeric vector) sequence of the number of chains; Default is 1:3
#' @param cutoff (numeric) where to cutoff chains in addition to the burnin; Default is 0
#'
#' @returns
#' @export
#'
#' @importFrom dplyr mutate slice summarize filter bind_rows
#' @importFrom tidyselect everything
#' @importFrom stats quantile sd
#' @importFrom tidyr unnest pivot_wider
#' @importFrom rstan Rhat
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom coda as.mcmc effectiveSize


chain_summary <- function(i,
                          samples,
                          chains = 1:3,
                          cutoff = 0) {

  s <- lapply(
    chains,
    FUN = function(x, index, cutoff) {
      as.data.frame(samples[[x]][, index]) %>%
        mutate(chain = x) %>%
        slice((cutoff + 1):n()) -> tmp

      colnames(tmp)[1] <- 'var1'
      return(tmp)
    },
    index = i,
    cutoff = cutoff
  )

  # summarize
  tmp <- bind_rows(s) %>%
          filter(.data$chain %in% chains) %>%
          summarize(mean = mean(.data$var1, na.rm = T),
                    lo = quantile(.data$var1, probs = 0.025, na.rm = T),
                    hi = quantile(.data$var1, probs = 0.975, na.rm = T),
                    lotail = mean(.data$var1) - sd(.data$var1),
                    hitail = mean(.data$var1) + sd(.data$var1)) %>%
          mutate(param = colnames(samples[[1]])[i],
                 param = gsub("[", '', .data$param, fixed = T),
                 param = gsub("]", '', .data$param, fixed = T))

  # get uncertainty
  denstails <- bind_rows(s) %>%
                filter(.data$chain %in% chains) %>%
                mutate(hi = case_when(var1 > tmp$hitail ~ 1, T ~ 0),
                       lo = case_when(var1 < tmp$lotail ~ 1, T ~ 0)) %>%
                summarize(hitail = mean(.data$hi), lotail = mean(.data$lo))

  tmp$hitail <- denstails$hitail
  tmp$lotail <- denstails$lotail

  tmp$unc.range <- tmp$hi - tmp$lo
  tmp$unc.rel <- (tmp$hi - tmp$lo) / tmp$mean

  # get rhat
  tmp1 <- bind_rows(s) %>%
            mutate(var1 = as.numeric(.data$var1)) %>%
            pivot_wider(names_from = 'chain',
                        names_prefix = 's',
                        values_from = 'var1',
                        values_fn = list) %>%
            unnest(cols = everything())

  tmp1 <- tmp1[, chains]
  rhat <- Rhat(as.matrix(tmp1))

  # get ESS
  bind_rows(s) %>%
    mutate(var1 = as.numeric(.data$var1)) %>%
    pull(.data$var1) -> tmp.ess

  # If there is missing data in the projections, ESS is NaN and breaks the loop here.
  if (all(is.nan(tmp.ess))) {
    ESS <- NA
  } else {
    as.mcmc(tmp.ess) %>%
      effectiveSize() %>%
      as.numeric() -> ESS
  }

  tmp <- tmp %>%
          select(.data$param, everything()) %>%
          mutate(rhat = rhat, ESS = ESS)

  return(tmp)
}
