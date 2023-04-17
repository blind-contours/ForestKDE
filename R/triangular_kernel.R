#' Triangular kernel
#'
#' This function calculates the triangular kernel for the given input data.
#'
#' @param x A vector of input values to compute the kernel.
#' @param data A matrix of data points.
#' @param weights A vector of weights associated with the data points.
#' @param bandwidth A numeric value specifying the bandwidth of the Gaussian kernel.
#'
#' @return The computed Triangular kernel value.
#' @examples
#' x <- c(1, 2, 3)
#' data <- c(1.2, 4.4, 0.11, 3.4, 9.9, 10.0, 3.4)
#' weights <- c(2, .4, .11, .4, .9, .0, .4)
#' bandwidth <- 1
#' triangular_kernel(x, data, weights, bandwidth)
#' @export

# Triangular kernel:

triangular_kernel <- function(x, data, weights, bandwidth) {
  dists <- as.matrix(dist(x = rbind(data, x), method = "euclidean"))
  dists <- dists[nrow(dists), 1:(nrow(dists) - 1)]
  u <- dists / bandwidth
  weights <- weights * ifelse(abs(u) <= 1, 1 - abs(u), 0)
  return(sum(weights) / sum(ifelse(abs(u) <= 1, 1 - abs(u), 0)))
}
