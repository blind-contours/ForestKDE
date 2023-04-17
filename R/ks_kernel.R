#' Kernel smoothing function using the 'ks' package
#'
#' This function calculates the kernel density estimation for the given input data using the 'ks' package.
#'
#' @param x A vector of input values to compute the kernel.
#' @param data A matrix of data points.
#' @param weights A vector of weights associated with the data points.
#' @param bandwidth A numeric value specifying the bandwidth of the kernel.
#'
#' @return The computed kernel density estimation value.
#' @importFrom ks kde
#' @examples
#' x <- c(1, 2, 3)
#' data <- c(1.2, 4.4, 0.11, 3.4, 9.9, 10.0, 3.4)
#' weights <- c(2, .4, .11, .4, .9, .0, .4)
#' bandwidth <- 1
#' ks_kernel(x, data, weights, bandwidth)
#' @export
ks_kernel <- function(x, data, weights, bandwidth) {
  kde_result <- suppressWarnings(kde(x = data, w = weights, h = bandwidth))
  return(predict(kde_result, x = x))
}
