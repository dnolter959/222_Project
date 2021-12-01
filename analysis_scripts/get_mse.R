#library(glmnet, lib = "/n/academic_homes/g_95193/u_386482_g_95193") #for server

library(glmnet)

get_mse <- function(beta_list, num_obs) {
  ## Takes a list of beta coefficients and returns
  ##beta_list is a numeric vector of beta coefficients
  ##num_obs is the number of observations in each dataframe
  num_test_set <- 1000
  
  xi_sd <-
    replicate(n = length(beta_list) - 1, rnorm(num_obs)) |> as.matrix()
  
  x = cbind(rep(1, num_obs), xi_sd)
  
  y = beta_list %*% as.matrix(t(x)) + rnorm(num_obs) # create matrix with TRUE Y
  
  
  test_xi_sd <-
    replicate(n = length(beta_list) - 1, rnorm(num_test_set)) |> as.matrix()
  
  test_x = cbind(rep(1, num_test_set), test_xi_sd)
  
  test_y = beta_list %*% as.matrix(t(test_x)) + rnorm(num_test_set) # create matrix with TRUE Y
  
  mat_to_fit <-
    cbind(y = t(y), xi_sd) #we don't want to fit the whole design matrix because then they'll try to fit 2 intercepts
  
  test_mat_to_fit <-
    cbind(test_y = t(test_y), test_xi_sd) #we don't want to fit the whole design matrix because then they'll try to fit 2 intercepts
  
  df_to_fit <- mat_to_fit |> data.frame()
  
  colnames(df_to_fit) <- paste0("col_", 1:ncol(df_to_fit))
  
  test_df_to_fit <- test_mat_to_fit |> data.frame()
  
  colnames(test_df_to_fit) <- paste0("col_", 1:ncol(df_to_fit))
  
  
  current_lm <- lm(col_1 ~ ., data = df_to_fit)
  
  lambda_lasso <- glmnet::cv.glmnet(
    y = mat_to_fit[, 1],
    x = mat_to_fit[, 2:ncol(mat_to_fit)],
    family = "gaussian",
    alpha = 1 #lasso penalty (0 is ridge)
  )$lambda.min
  
  lambda_elastic <- glmnet::cv.glmnet(
    y = mat_to_fit[, 1],
    x = mat_to_fit[, 2:ncol(mat_to_fit)],
    family = "gaussian",
    alpha = .5 #lasso penalty (0 is ridge)
  )$lambda.min
  
  
  lambda_ridge <- glmnet::cv.glmnet(
    y = mat_to_fit[, 1],
    x = mat_to_fit[, 2:ncol(mat_to_fit)],
    family = "gaussian",
    alpha = 0 #ridge penalty
  )$lambda.min
  
  current_lasso <- glmnet::glmnet(
    y = mat_to_fit[, 1],
    x = mat_to_fit[, 2:ncol(mat_to_fit)],
    family = "gaussian",
    alpha = 1,
    #1 is lasso penalty (0 is ridge)
    lambda = lambda_lasso
  )
  
  current_ridge <- glmnet::glmnet(
    y = mat_to_fit[, 1],
    x = mat_to_fit[, 2:ncol(mat_to_fit)],
    family = "gaussian",
    alpha = 0,
    #0 is ridge
    lambda = lambda_ridge
    
  )
  
  current_elastic <- glmnet::glmnet(
    y = mat_to_fit[, 1],
    x = mat_to_fit[, 2:ncol(mat_to_fit)],
    family = "gaussian",
    alpha = .5,
    #0 is ridge
    lambda = lambda_elastic
  )
  
  return_vector <- c()
  
  p <- length(beta_list - 1)
  
  #y = y / sd(y) #normalize Y so that MSE measurements are in the same ballpark
  #I don't think we want to do this.
  test_y <-
    test_y |> as.vector() #so we can do sums of that and our predictions
  for (mod in list(current_lasso, current_ridge, current_elastic)) {
    current_preds <-
      predict(mod, test_mat_to_fit[, 2:ncol(test_mat_to_fit)]) |> as.vector()
    current_mse <-
      sum((current_preds - test_y) ^ 2) / (num_test_set)
    return_vector <- c(return_vector, current_mse)
  }
  
  ## for lm
  current_preds <- predict.lm(current_lm,
                              newdata = test_df_to_fit) |> as.vector()
  
  current_mse <- sum((current_preds - test_y) ^ 2) / (num_test_set)
  return_vector <- c(return_vector, current_mse)
  
  highest_beta <- (length(beta_list) - 1)
  
  names(return_vector) <- c("LASSO",
                            "Ridge",
                            "Elastic",
                            "Unpenalized")
  
  return(return_vector)
}
