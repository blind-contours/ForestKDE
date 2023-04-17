#' Cross-Validated Random Forest SRC with Parallelization
#'
#' Performs a k-fold cross-validated random forest SRC with parallelization to find the best hyperparameters and kernel.
#'
#' @param train_data A data frame containing the training data.
#' @param outcome_col A character string specifying the column name of the outcome variable.
#' @param k_folds An integer specifying the number of folds for k-fold cross-validation (default is 10).
#' @param ntree_grid A vector of integers specifying the grid of ntree values to search.
#' @param mtry_grid A vector of integers specifying the grid of mtry values to search.
#' @param nodesize_grid A vector of integers specifying the grid of nodesize values to search.
#' @param splitrule_grid A character vector specifying the grid of splitrule values to search.
#' @param kernel_grid A character vector specifying the grid of kernel names to search.
#' @param bandwidth_grid A list of numeric vectors specifying the grid of bandwidth values to search for each kernel.
#'
#' @return A list containing the best negative log-likelihood (best_nll), best ntree (best_ntree), best mtry (best_mtry),
#' best nodesize (best_nodesize), best splitrule (best_splitrule), best fitted model (best_model), and best kernel (best_kernel).
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom stats predict as.formula
#' @import randomForestSRC
#' @import foreach

#' @export
#' @examples

cv_rfsrc_parallel_nll <- function(train_data, outcome_col, k_folds = 10, ntree_grid, mtry_grid, nodesize_grid, splitrule_grid, kernel_grid, bandwidth_grid) {

  n_obs <- nrow(train_data)
  fold_size <- floor(n_obs / k_folds)

  best_nll <- Inf
  best_ntree <- NA
  best_mtry <- NA
  best_nodesize <- NA
  best_splitrule <- NA
  best_model <- NULL
  best_kernel <- NA

  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)

  results <- foreach(ntree_val = ntree_grid, .combine = 'rbind', .packages = c('ForestKDE', 'randomForestSRC', 'matrixStats', 'ks')) %dopar% {


    for (mtry_val in mtry_grid) {
      for (nodesize_val in nodesize_grid) {
        for (splitrule_val in splitrule_grid) {
          nll_list <- matrix(0, nrow = k_folds, ncol = length(kernel_grid))
          bandwidth_list <-  matrix(0, nrow = k_folds, ncol = length(kernel_grid))

          for (fold in 1:k_folds) {
            start_idx <- (fold - 1) * fold_size + 1
            end_idx <- start_idx + fold_size - 1

            if (fold == k_folds) {
              end_idx <- n_obs
            }

            val_idx <- start_idx:end_idx
            train_idx <- setdiff(seq_len(n_obs), val_idx)

            at <- train_data[train_idx, ]
            av <- train_data[val_idx, ]

            rf_model <- rfsrc(as.formula(paste(outcome_col, "~ .")), data = at, ntree = ntree_val, mtry = mtry_val, nodesize = nodesize_val, splitrule = splitrule_val)
            val_prox_matrix <- predict(rf_model, av, proximity = TRUE)$proximity
            densities_rf_list <- calc_conditional_density(prox_matrix = val_prox_matrix, data = av, A_col = outcome_col, bandwidth_grid = bandwidth_grid)

            nll_list[fold,] <- sapply(densities_rf_list$kernel_values, function(densities_rf) {
              return(-sum(log(densities_rf)))
            })

            bandwidth_list[fold,] <- unlist(densities_rf_list$best_bandwidth_index)
          }

          nll_avg_list <- apply(nll_list, 2, function(x) mean(x, na.rm = TRUE))
          kernel_idx <- which.min(nll_avg_list)
          nll_avg <- nll_avg_list[kernel_idx]
          bandwidth_kernel_best <- mean(bandwidth_list[,kernel_idx])

          if (nll_avg < best_nll) {
            best_nll <- nll_avg
            best_ntree <- ntree_val
            best_mtry <- mtry_val
            best_nodesize <- nodesize_val
            best_splitrule <- splitrule_val
            best_model <- rf_model
            best_kernel  <- kernel_grid[kernel_idx]
            best_bandwidth <- bandwidth_kernel_best
          }
        }
      }
    }

    data.frame(best_nll = best_nll, best_ntree = best_ntree, best_mtry = best_mtry, best_nodesize = best_nodesize, best_splitrule = best_splitrule, best_kernel = best_kernel, best_bandwidth = best_bandwidth)
  }

  stopCluster(cl)

  best_params <- results[which.min(results$best_nll),]
  best_model <- rfsrc(as.formula(paste(outcome_col, "~ .")), data = train_data, ntree = best_params$best_ntree, mtry = best_params$best_mtry, nodesize = best_params$best_nodesize, splitrule = best_params$best_splitrule)

  return(list(best_nll = best_params$best_nll, best_ntree = best_params$best_ntree, best_mtry = best_params$best_mtry, best_nodesize = best_params$best_nodesize, best_splitrule = best_params$best_splitrule, best_model = best_model, best_kernel = best_params$best_kernel, best_bandwidth = best_params$best_bandwidth))
}
