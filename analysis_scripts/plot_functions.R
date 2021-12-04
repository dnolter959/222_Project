format_df <- function(input_df){
  ## takes a dataframe returned by replicating model_fit and transforms it so
  ## there is one row per estimate 
  data <- input_df %>% 
    mutate(sim_number = 1:nrow(input_df)) %>% #we have to do this to get pivots to recognize unique rows
    tidyr::pivot_longer(cols = colnames(input_df), names_to = "Estimate", values_to = "Value") %>% 
    separate(col = "Estimate", into = c("Model", "Beta")) %>%
    unite(col = Model_iter, Model, sim_number, sep = "_") %>%
    pivot_wider(names_from = "Beta", values_from = "Value", names_prefix = "Beta_") %>%
    separate(col = Model_iter, into = c("Model", "Simulation")) %>%
    data.frame() %>% 
    rename(bias = Beta_bias, variance = Beta_variance , mse = Beta_mse )
  return(data)
}

beta_boxplots <- function(input_df, format_data = F, beta_num, beta_list, legend = T){ 
  #' Creates a boxplot using each 
  #' input_df: the dataframe (either returned by replicating model_fit or format_df)
  #' format_data: a logical indicating whether input_df needs to be formatted
  #' beta_num: the beta coefficient you want boxplots of
  #' beta_list: the list of beta coefficients
  #' legend: a logical indicating whether we need to include a legend in this plot
  
  if(!format_data){
    data <- input_df
  }
  else{
    data <- format_df(input_df)
  }
  
  my_list <- data[,paste('Beta', beta_num, sep = "_")]
  truth <- beta_list[beta_num + 1] #we want beta_0 to be the intercept
  
  p <- ggplot(data = data, mapping = aes(col = Model, y = my_list)) + 
    geom_boxplot() +
    geom_hline(aes(yintercept = truth), col = "blue") +
    labs(y = "", title = paste('Beta', beta_num, sep = "_")) + 
    scale_x_continuous(breaks = c())
  
  if(!legend){
    p <- p + theme(legend.position = "none")
  }
  return(p)
  
}

plot_all_beta <- function(input_df, beta_list, ncol = 4){
  #' a function taking a dataframe (returned by replicating model_fit) which returns boxplots
  #' input_df: a dataframe created by replicating model_fit
  #' beta_list: the list of true beta coefficients
  #' ncol: the desired number of columns in the resulting plot.
  df <- format_df(input_df)
  
  
  beta_plots <- list()
  for (i in 0:(length(beta_list) - 1)){
    beta_plots[[i + 1]] <- beta_boxplots(input_df = df, format_data = F, beta_num = i, beta_list = beta_list, legend = F)
    beta_plots[[i + 1]]
  }
  beta_plots[[length(beta_list) + 1]] <- beta_boxplots(input_df = df, format_data = F, beta_num = i, beta_list = beta_list, legend = T) %>%  
    ggpubr::get_legend() %>% 
    as_ggplot()
  
  egg::ggarrange(plots = beta_plots, ncol = ncol)
}
