true_density_mixture <- function(W, A, prob, mu1, sd1, mu2, sd2) {
  n <- length(W)
  density_values <- numeric(n)

  for (i in 1:n) {
    density_values[i] <- prob * dnorm(A[i], mean = mu1 + 0.5 * W[i], sd = sd1) + (1 - prob) * dnorm(A[i], mean = mu2 + 1.5 * W[i], sd = sd2)
  }

  return(density_values)
}
