#' Calculate conditional density
#'
#' This function computes the conditional density for a given proximity matrix, data, column of interest, and bandwidth grid.
#'
#' @param prox_matrix A proximity matrix from the random forest model.
#' @param data A data.frame containing the input dataset.
#' @param A_col A character specifying the column of interest in the dataset for density calculation.
#' @param bandwidth_grid A list containing bandwidth grids for each kernel function.
#'
#' @return A list of conditional density values calculated using different kernel functions.
#' @examples
#' prox_matrix <- matrix(runif(9, 0, 1), nrow = 3)
#' data <- data.frame(A = c(1, 2, 3), B = c(2, 3, 4))
#' A_col <- "A"
#' bandwidth_grid <- list(gaussian_kernel = c(0.5, 1), triangular_kernel = c(0.5, 1))
#'
#' calc_conditional_density(prox_matrix, data, A_col, bandwidth_grid)
#' @export
calc_conditional_density <- function(prox_matrix, data, A_col, bandwidth_grid) {
  A <- data[[A_col]]
  n <- nrow(prox_matrix)

  # Initialize lists to store density values for each kernel
  kernel_density_values_list <- list()
  best_bandwidth_values_list <- list()

  for (kernel_name in names(bandwidth_grid)) {
    kernel_function <- get(kernel_name)
    kernel_bandwidth_grid <- bandwidth_grid[[kernel_name]]

    kernel_density_values <- sapply(kernel_bandwidth_grid, function(bandwidth) {
      density_values <- numeric(n)

      for (i in 1:n) {
        weights <- prox_matrix[i, ]
        weights[weights == 0] <- 1/sqrt(n)

        density_values[i] <- kernel_function(x = A[i], data = A, weights = weights, bandwidth = bandwidth)
      }

      return(density_values)
    })

    lowest_nll_i <- which.min(apply(kernel_density_values, 2, function(x) {
      return(-sum(log(x)))
    }))

    kernel_density_values_list[[kernel_name]] <- kernel_density_values[,lowest_nll_i]
    best_bandwidth_values_list[[kernel_name]] <- lowest_nll_i
  }

  return(list("kernel_values" = kernel_density_values_list, "best_bandwidth_index" = best_bandwidth_values_list))
}
