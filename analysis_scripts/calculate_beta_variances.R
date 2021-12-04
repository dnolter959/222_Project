calculate_beta_variances <- function(df){
  fdf <- format_df(df)
  
  means <- fdf %>%
    select(-c("bias", "variance", "mse", "Simulation")) %>% 
    group_by(Model) %>%
    dplyr::summarise(across(everything(), .fns = mean, na.rm= TRUE)) 
  
  beta_vars <- fdf %>%
    group_by(Model) %>%
    select(-c("bias", "variance", "mse", "Simulation")) %>% 
    dplyr::summarise(across(everything(), 
                            .fns = ~ sum((.x-mean(.x))^2)/length(.x) ) )
  
  beta_bias <- sapply(1:nrow(means), function(x){
    unlist(means[x,-1]) - beta_list}) %>%   
    t() %>%  
    cbind(means[,1], . )
  
  beta_mse <- sapply(1:nrow(means), function(x){
    beta_bias[x, -1]^2 + beta_vars[x, -1]
  }) %>% 
    t() %>%  
    cbind(means[,1], . ) 
  
  means <- fdf %>%
    select(-c("bias", "variance", "mse", "Simulation")) %>% 
    group_by(Model) %>%
    dplyr::summarise(across(everything(), .fns = mean, na.rm= TRUE)) 
  
  beta_vars %>% tidyr::pivot_longer(cols = colnames(beta_mse)[-1], names_to = "beta_num", values_to = "value") %>% 
    separate(beta_num, into = c("junk", "beta_num")) %>% 
    mutate(beta_num = as.numeric(beta_num)) %>% 
    mutate(short_model = substr(Model, start = 1, stop = 2)) %>% 
    arrange(beta_num) %>% 
    mutate(is_zero = (beta_list[beta_num + 1] == 0))
}

