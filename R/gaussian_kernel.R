#' Gaussian (Radial basis function) kernel
#'
#' This function calculates the Gaussian kernel for the given input data.
#'
#' @param x A vector of input values to compute the kernel.
#' @param data A matrix of data points.
#' @param weights A vector of weights associated with the data points.
#' @param bandwidth A numeric value specifying the bandwidth of the Gaussian kernel.
#'
#' @return The computed Gaussian kernel value.
#' @examples
#' x <- c(1, 2, 3)
#' data <- c(3.3, 0, 4.1, 0, 0, 2.2, 1, 4)
#' weights <- c(0.22, 0, 0, 0, 0, 0.11, 10.4, 0.7)
#' bandwidth <- 1
#' gaussian_kernel(x, data, weights, bandwidth)
#' @export

# Gaussian (Radial basis function) kernel:
gaussian_kernel <- function(x, data, weights, bandwidth) {
  dists <- as.matrix(dist(x = rbind(data, x), method = "euclidean"))
  dists <- dists[nrow(dists), 1:(nrow(dists) - 1)]
  epsilon <- 1e-10
  weights_updated <- weights * exp(-dists^2 / (2 * bandwidth^2))
  return(sum(weights_updated) / sum(exp(-dists^2 / (2 * bandwidth^2)) + epsilon))
}
