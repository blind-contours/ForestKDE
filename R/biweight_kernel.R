#' Biweight (quartic) kernel
#'
#' This function calculates the biweight (quartic) kernel for the given input data.
#'
#' @param x A vector of input values to compute the kernel.
#' @param data A matrix of data points.
#' @param weights A vector of weights associated with the data points.
#' @param bandwidth A numeric value specifying the bandwidth of the biweight kernel.
#' @importFrom stats dist
#'
#' @return The computed biweight kernel value.
#' @examples
#' x <- c(1, 2, 3)
#' data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
#' weights <- c(0.5, 0.3, 0.2)
#' bandwidth <- 1
#'
#' biweight_kernel(x, data, weights, bandwidth)
#' @export
biweight_kernel <- function(x, data, weights, bandwidth) {
  dists <- as.matrix(dist(x = rbind(data, x), method = "euclidean"))
  dists <- dists[nrow(dists), 1:(nrow(dists) - 1)]
  u <- dists / bandwidth
  weights <- weights * ifelse(abs(u) <= 1, (15/16) * (1 - u^2)^2, 0)
  return(sum(weights) / sum(ifelse(abs(u) <= 1, (15/16) * (1 - u^2)^2, 0)))
}
