run_simulation <- function(n, data, train_data_ratio, ntree_grid, mtry_grid, nodesize_grid, splitrule_grid, kernel_grid) {
  # Split the data into training and validation sets
  n_train <- floor(n * train_data_ratio)
  train_idx <- sample(seq_len(n), size = n_train, replace = FALSE)
  at <- data[train_idx, ]
  av <- data[-train_idx, ]

  # Find the best model using cross-validation
  best_params_and_model <- cv_rfsrc_parallel_nll(train_data = at, outcome_col = "A", k_folds = 10, ntree_grid = ntree_grid, mtry_grid = mtry_grid, nodesize_grid = nodesize_grid, splitrule_grid = splitrule_grid, kernel_grid = kernel_grid, bandwidth_grid = bandwidth_grid)

  # Extract the proximity matrix from the random forest model
  val_prox_matrix <- predict(best_params_and_model$best_model, av, proximity = TRUE)$proximity

  # Estimate the conditional density of A given W for each observation
  densities_rf <- calc_conditional_density(prox_matrix = val_prox_matrix, data = av, A_col = "A", kernel_function = best_params_and_model$best_kernel, best_params_and_model$)

  # Calculate the true density for validation set
  true_density_values <- true_density(av$W, av$A)

  # Calculate the mean integrated squared error for the random forest model
  mise_rf <- mean((true_density_values - densities_rf)^2, na.rm = TRUE)

  # Fit haldensify on the same data
  haldensify_fit <- haldensify(
    A = at$A, W = at$W, n_bins = 10L, lambda_seq = exp(seq(-1, -10, length = 100)),
    # the following arguments are passed to hal9001::fit_hal()
    max_degree = 1
  )

  # Calculate the estimated densities using haldensify
  densities_hal <- predict(haldensify_fit, new_A = av$A, new_W = av$W)

  # Calculate the mean integrated squared error for the haldensify model
  mise_hal <- mean((true_density_values - densities_hal)^2)

  # Return a data frame with the results
  results <- data.frame(
    n = n,
    train_data_ratio = train_data_ratio,
    best_nll = best_params_and_model$best_nll,
    best_ntree = best_params_and_model$best_ntree,
    best_mtry = best_params_and_model$best_mtry,
    best_nodesize = best_params_and_model$best_nodesize,
    best_splitrule = best_params_and_model$best_splitrule,
    mise_rf = mise_rf,
    mise_hal = mise_hal
  )

  return(results)
}
