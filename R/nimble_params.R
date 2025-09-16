#' Set up nimble parameters
#'
#' @description Sets up vector of parameters to trace
#'
#' @param data (list) data formatted for nimble
#' @param constants (list) constants formatted for nimble
#' @param sp.auto (logical) whether to trace parameters associated with spatial model (T) or not (F)
#' @param lambda (logical) whether to trace lambda (T) or not (F)
#' @param XB (logical) whether to trace XB (T) or not (F)
#' @param effort (logical) whether to trace effort parameters (T) or not (F)
#'
#' @returns A vector of parameters to trace
#' @export
#'
#' @importFrom tidyselect everything
#'
#' @examples
#'\dontrun{
#'
#'params <- nimble_params(data,
#'                        constants,
#'                        lambda = T,
#'                        XB = T,
#'                        sp.auto = T,
#'                        effort = F)
#'
#'}



nimble_params <- function(data,
                          constants,
                          sp.auto,
                          lambda = T,
                          XB = T,
                          effort = F) {

  # always trace these
  params <- c("B", "alpha")

  # add spatial model parameters
  if (sp.auto == T) params <- c(params, "spat", "tau")

  # add lambda if you want
  if (lambda == T) {
    params <- c(params, "lambda0")
  }

  # add XB if you want
  if (XB == T) {
    params <- c(params, "XB0")
  }

  # add dataset-specific parameters
  for (d in 1:constants$nD) {

    if (paste0("Xy", d) %in% names(data)) {
      # count

      if (constants[[paste0("nCovY", d)]] > 0) {
        params <- c(params, paste0("C", d))
      } else {
        params <- c(params, paste0("p", d))
      }


    } else if (paste0("Xw", d) %in% names(data)) {
      # PO

      if (constants[[paste0("nCovW", d)]] > 0) {
        params <- c(params, paste0("A", d))
      } else {
        params <- c(params, paste0("p", d))
      }

      if (effort == T) {
        params <- c(params, paste0("E", d))
      }

    } else if (paste0("Xv", d) %in% names(data)) {
      # DND

      if (constants[[paste0("nCovV", d)]] > 0) {
        params <- c(params, paste0("D", d))
      } else {
        params <- c(params, paste0("p", d))
      }
    }

  } # end loop through datasets


  return(params)
}
