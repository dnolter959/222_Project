model_fit <- function(beta_list, num_obs) {
  mat_to_fit <- generate_data(beta_list, num_obs)
  
  current_lm <- lm(mat_to_fit[,1] ~ mat_to_fit[,2:ncol(mat_to_fit)])
  current_lm <- lm(mat_to_fit[,1] ~ mat_to_fit[,2:ncol(mat_to_fit)])
  
  lambda_lasso <- glmnet::cv.glmnet(y = mat_to_fit[,1], 
                                    nfolds = 5, #so it will be ok when we use on small samples
                                    x = mat_to_fit[,2:ncol(mat_to_fit)],
                                    family = "gaussian",
                                    alpha = 1 #lasso penalty (0 is ridge)
  ) %>% 
    .$lambda.min
  
  lambda_elastic <- glmnet::cv.glmnet(y = mat_to_fit[,1], 
                                      x = mat_to_fit[,2:ncol(mat_to_fit)],
                                      nfolds = 5,
                                      family = "gaussian",
                                      alpha = .5 #lasso penalty (0 is ridge)
  ) %>% 
    .$lambda.min
  
  
  lambda_ridge <- glmnet::cv.glmnet(y = mat_to_fit[,1], 
                                    x = mat_to_fit[,2:ncol(mat_to_fit)],
                                    nfolds = 5,
                                    family = "gaussian",
                                    alpha = 0 #ridge penalty
  ) %>% 
    .$lambda.min
  
  current_lasso <- glmnet::glmnet(y = mat_to_fit[,1], 
                                  x = mat_to_fit[,2:ncol(mat_to_fit)],
                                  family = "gaussian",
                                  alpha = 1, #1 is lasso penalty (0 is ridge)
                                  lambda = lambda_lasso
  )
  
  current_ridge <- glmnet::glmnet(y = mat_to_fit[,1], 
                                  x = mat_to_fit[,2:ncol(mat_to_fit)],
                                  family = "gaussian",
                                  alpha = 0, #0 is ridge
                                  lambda = lambda_ridge
                                  
  )
  
  current_elastic <- glmnet::glmnet(y = mat_to_fit[,1], 
                                    x = mat_to_fit[,2:ncol(mat_to_fit)],
                                    family = "gaussian",
                                    alpha = .5, #0 is ridge
                                    lambda = lambda_elastic)
  
  return_vector <- c()
  
  p <- length(beta_list - 1)
  y <- y %>% as.vector() #so we can do sums of that and our predictions
  for (mod in list(current_lasso, current_ridge,current_elastic)){
    current_preds <- predict(mod, mat_to_fit[,2:ncol(mat_to_fit)] ) %>% as.vector()
    current_bias <- mean(current_preds - y)
    current_var <- var(current_preds)
    current_mse <- sum((current_preds - y)^2) / (num_obs - p - 1)
    return_vector <- c(return_vector, as.vector(coef(mod)), current_bias, current_var, current_mse)
  }
  
  ## for lm
  current_preds <- predict(current_lm, mat_to_fit[,2:ncol(mat_to_fit)] %>% data.frame() ) %>% as.vector()
  current_bias <- mean(current_preds - y)
  current_var <- var(current_preds)
  current_mse <- sum((current_preds - y)^2) / (num_obs - p - 1)
  return_vector <- c(return_vector, as.vector(coef(current_lm)), current_bias, current_var, current_mse)
  
  highest_beta <- (length(beta_list) - 1)
  
  names(return_vector) <- c(paste("LASSO", 0:highest_beta, sep = '_'),
                            paste("LASSO", c("bias", "variance", "mse"), sep = '_'),
                            paste("Ridge", 0:highest_beta, sep = '_'),
                            paste("Ridge", c("bias", "variance", "mse"), sep = '_'),
                            paste("Elastic", 0:highest_beta, sep = '_'),
                            paste("Elastic", c("bias", "variance", "mse"), sep = '_'),
                            paste("Unpenalized", 0:highest_beta, sep = '_'),
                            paste("Unpenalized", c("bias", "variance", "mse"), sep = '_')
  )
  
  return(return_vector)
}
