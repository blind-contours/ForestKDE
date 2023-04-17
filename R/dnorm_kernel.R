#' Dnorm kernel function
#'
#' This function computes the dnorm kernel using a Gaussian (normal) distribution with input data and associated weights.
#'
#' @param x A vector of input values to compute the kernel.
#' @param data A matrix of data points.
#' @param weights A vector of weights associated with the data points.
#' @param bandwidth A numeric value specifying the bandwidth of the kernel (not used in this function but kept for consistency with other kernels).
#' @importFrom stats dnorm weighted.mean
#' @importFrom matrixStats weightedVar
#' @return The computed dnorm kernel value.
#' @export
#' @examples
#' x <- c(1, 2, 3)
#' data <- c(3.3, 0, 4.1, 0, 0, 2.2, 1, 4)
#' weights <- c(0.22, 0, 0, 0, 0, 0.11, 10.4, 0.7)
#' bandwidth <- 1
#' dnorm_kernel(x, data, weights, bandwidth)
#' @export
dnorm_kernel <- function(x, data, weights, bandwidth) {
  return(dnorm(x, mean = weighted.mean(data, weights), sd = sqrt(weightedVar(data, weights))))
}
