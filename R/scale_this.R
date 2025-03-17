#' Scale values
#'
#' @description A function to re-scale values to a distribution with mean 0 and standard deviation 1
#'
#' @param x A vector of values to re-scale
#'
#' @returns A vector of scaled values with mean 0 and standard deviation 1
#'
#' @examples
#' x <- rnorm(1000, mean = 10, sd = 5)
#'
#' scale_this(x)
#'


scale_this <- function(x) {
  (x - mean(x, na.rm=T))/sd(x, na.rm=T)
}
