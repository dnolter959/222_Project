args <- commandArgs(T)


index <- as.numeric(args[2])
#print(index)
set.seed(index) #make sure we're not just doing the same thing every time

source("get_mse.R")

n_reps <-
  as.numeric(args[1]) #so we can test this on my local with a small number

n_covariates <- 100
sparsity_levels <-
  c(.1, .5, .98) #this needs to work with prop_coefs_small to get whole numbers.
prop_coefs_small_levels <- c(0, .5, 1)

# sapply(n_covariates*sparsity_levels, function(x){`*`(x,prop_coefs_small)}) # needs to return whole numbers

small_coefs <- runif(200, min = 0.01, max = 0.1)
medium_coefs <- runif(200, min = 0.5, max = 1.5)

num_obs_vec <-
  c(200, 800, 1600) #We will have at least 2 observations per covariate

#the rate-limiting factor here will not be the for loops, so I'm not going to bother vectorizing here.

for (sparsity in sparsity_levels) {
  for (prop_small in prop_coefs_small_levels) {
    num_non_zero <- n_covariates * (1 - sparsity)
    num_small <- num_non_zero * prop_small
    num_medium <- num_non_zero * (1 - prop_small)
    num_zero <- n_covariates * sparsity
    
    #print(paste("sparsity: ", sparsity, "small: ", prop_small))
    
    
    beta_list <- c(
      0, #We will always set our intercept to 0
      sample(medium_coefs, size = num_medium, replace = F),
      sample(small_coefs, size = num_small, replace = F),
      rep(0, num_zero) #fill the rest with zeroed coefficients
    )
    
    #print(paste("small", sum(beta_list < 0.4 & beta_list > 0), "medium", sum(beta_list > 0.4), "zero", sum(beta_list == 0)))
    
    for (num_obs in num_obs_vec) {
      df <- replicate(n_reps, get_mse(beta_list, num_obs)) |> t() |> data.frame()
      df$sparsity <- sparsity
      df$prop_small <- prop_small
      df$num_obs <- num_obs
      if (!exists("full_df")) {
        full_df <- df
      }
      else{
        full_df <- rbind(full_df, df)
      }
    }
  }
}

save(full_df, file = paste0("mse", index, ".Rdata"))
