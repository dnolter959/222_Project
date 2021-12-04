## Takes a list of beta coefficients and returns a matrix satisfying those constraints
generate_data <- function(
  beta_list, # a numeric vector of beta coefficients
  num_obs, ## number of observations in the output matrix
  num_underlying_factors = 0, #number of underlying factors (0 is no multicollinearity)
  covariates_per_factor = 0, #covariates per underlying factor
  strength_collinearity = 0 #between 0 and 1, 1 is perfect collinearity
){
  
  xi_sd <- replicate(n =length(beta_list) - 1, rnorm(num_obs)) %>% as.matrix()
  if(num_underlying_factors > 0 && covariates_per_factor > 1 && strength_collinearity > 0){
    k <- 2
    for (i in 1:num_underlying_factors){
      current_factor <- rnorm(num_obs)
      new_k <- k + covariates_per_factor
      if (new_k > ncol(xi_sd)){
        k <- 3 #we've already gone around once so we might as well mix up some of the collinearity groups
        new_k <- k + covariates_per_factor
      }
      
      
      current_to_add <- k:new_k
      for (j in current_to_add){
        xi_sd[,j] <- (xi_sd[,j]*(1 - strength_collinearity) + current_factor*strength_collinearity ) |> scale() #weight them and then normalize back to have a sd of 1. We will be dividing 
      }
    }
  }
  x = cbind(rep(1, num_obs), xi_sd)
  y = beta_list %*% as.matrix(t(x)) + rnorm(num_obs) # create matrix with TRUE Y
  
  mat_to_fit <- cbind(y = t(y), xi_sd) 
  return(mat_to_fit)
}
