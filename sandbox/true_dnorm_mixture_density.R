true_dnorm_mixture_density <- function(W, A) {
  n <- length(W)
  density_values <- numeric(n)

  for (i in 1:n) {
    if (W[i] < 0) {
      density_values[i] <- dnorm(A[i], mean = 1 + 0.5 * W[i], sd = 0.5)
    } else {
      density_values[i] <- dnorm(A[i], mean = -1 + 1.5 * W[i], sd = 0.8)
    }
  }

  return(density_values)
}
