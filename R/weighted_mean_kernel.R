#' Weighted mean kernel function
#'
#' This function computes the weighted mean kernel using a Gaussian (normal) distribution with input data and associated weights.
#'
#' @param x A vector of input values to compute the kernel.
#' @param data A matrix of data points.
#' @param weights A vector of weights associated with the data points.
#' @param bandwidth A numeric value specifying the bandwidth of the kernel.
#'
#' @return The computed weighted mean kernel value.
#' @examples
#' x <- c(1, 2, 3)
#' data <- c(1.2, 4.4, 0.11, 3.4, 9.9, 10.0, 3.4)
#' weights <- c(2, .4, .11, .4, .9, .0, .4)
#' bandwidth <- 1
#' weighted_mean_kernel(x, data, weights, bandwidth)
#' @export
weighted_mean_kernel <- function(x, data, weights, bandwidth) {
  return(weighted.mean(dnorm(x, mean = data, sd = sqrt(weightedVar(data, weights * length(data)) / sum(weights ^ 2))), weights))
}
