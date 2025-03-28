#' Title
#'
#' @param i
#' @param samples
#' @param chains
#' @param cutoff
#'
#' @returns
#' @export
#'
#' @importFrom dplyr mutate slice summarize filter bind_rows
#' @importFrom stats quantile sd
#' @importFrom tidyr unnest
#' @importFrom rstan Rhat
#' @importFrom coda as.mcmc effectiveSize
#'
#' @examples


chain_summary <- function(i,
                          samples,
                          chains = 1:3,
                          cutoff = 0) {
  s <- lapply(
    chains,
    FUN = function(x, index, cutoff) {
      as.data.frame(samples[[x]][, index]) %>%
        dplyr::mutate(chain = x) %>%
        dplyr::slice((cutoff + 1):nrow(.)) -> tmp

      colnames(tmp)[1] <- 'var1'
      return(tmp)
    },
    index = i,
    cutoff = cutoff
  )

  # summarize
  tmp <- dplyr::bind_rows(s) %>%
          dplyr::filter(chain %in% chains) %>%
          dplyr::summarize(
            mean = mean(var1, na.rm = T),
            lo = stats::quantile(var1, probs = 0.025, na.rm = T),
            hi = stats::quantile(var1, probs = 0.975, na.rm = T),
            lotail = mean(var1) - stats::sd(var1),
            hitail = mean(var1) + stats::sd(var1)
          ) %>%
          mutate(
            param = colnames(samples[[1]])[i],
            param = gsub("[", '', param, fixed = T),
            param = gsub("]", '', param, fixed = T)
          )


  # get uncertainty
  denstails <- dplyr::bind_rows(s) %>%
                dplyr::filter(chain %in% chains) %>%
                dplyr::mutate(hi = dplyr::case_when(var1 > tmp$hitail ~ 1, T ~ 0),
                              lo = dplyr::case_when(var1 < tmp$lotail ~ 1, T ~ 0)) %>%
                dplyr::summarize(hitail = mean(hi), lotail = mean(lo))

  tmp$hitail <- denstails$hitail
  tmp$lotail <- denstails$lotail

  tmp$unc.range <- tmp$hi - tmp$lo
  tmp$unc.rel <- (tmp$hi - tmp$lo) / tmp$mean

  # get rhat
  tmp1 <- dplyr::bind_rows(s) %>%
            dplyr::mutate(var1 = as.numeric(var1)) %>%
            dplyr::pivot_wider(.,
                               names_from = 'chain',
                               names_prefix = 's',
                               values_from = 'var1',
                               values_fn = list
                              ) %>%
                              tidyr::unnest(cols = everything())
  tmp1 <- tmp1[, chains]
  rhat <- rstan::Rhat(as.matrix(tmp1))

  # get ESS
  dplyr::bind_rows(s) %>%
    dplyr::mutate(var1 = as.numeric(var1)) %>%
    dplyr::pull(var1) -> tmp.ess

  # If there is missing data in the projections, ESS is NaN and breaks the loop here.
  if (all(is.nan(tmp.ess))) {
    ESS <- NA
  } else {
    coda::as.mcmc(tmp.ess) %>%
      coda::effectiveSize() %>%
      as.numeric() -> ESS
  }

  tmp <- tmp %>%
          dplyr::select(param, everything()) %>%
          dplyr::mutate(rhat = rhat, ESS = ESS)

  return(tmp)
}
