#' Extract and calculate derived quantities for projections
#'
#' @param i (numeric) projection number
#' @param data (NIMBLE data) data created and formatted for NIMBLE
#' @param beta (matrix) beta estimates extracted from MCMC samples
#' @param spat (matrix) spatial effect estimates extracted from MCMC samples
#' @param lambda0 (matrix) lambda estimates extracted from MCMC samples
#'
#' @returns A data.frame of lambda, psi, and XB estimates for projection i
#'
#' @importFrom VGAM clogloglink
#' @importFrom dplyr bind_cols


get_projections <- function(i, data, beta, spat = NULL, lambda0) {
  # For each projection, calculate XB, lambda, and psi
  which.Xz <- paste0('Xz', i)

  # Calculate X*beta
  XB <- beta %*% t(as.matrix(data[[which.Xz]]))

  if (is.null(spat)) {
    lam <- exp(XB)
  } else {
    lam <- exp(XB + spat)
  }

  # Calculate projected lambda
  lam <- as.data.frame(lam)
  colnames(lam) <- colnames(lambda0)
  colnames(lam) <- gsub('lambda0', paste0('lambda', i), colnames(lam))

  # Calculate projected psi
  psi <- lapply(lam, function(x)
          clogloglink(log(x), inverse = T)) %>%
          data.frame()
  names(psi) <- gsub(paste0('lambda', i, '.'), paste0('psi', i, '['), names(psi))
  names(psi) <- gsub('[.]', ']', names(psi))


  # Calculate XB for projections
  XB <- as.data.frame(XB)
  colnames(XB) <- colnames(lambda0)
  colnames(XB) <- gsub('lambda0', paste0('XB', i), colnames(XB))


  return(bind_cols(lam, psi, XB))
}
