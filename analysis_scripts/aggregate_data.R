setwd('slurm_results') #move to subdirectory

if (exists("aggregate_df")){
  rm("aggregate_df")
}
if (exists("full_df")){
  rm("full_df")
}

for (filename in list.files()) {
  load(filename) #will create a new exceed_vec object each time
  if (!exists("aggregate_df")) { #we haven't created the matrix yet
    aggregate_df <- full_df
  }
  else{
    aggregate_df <- rbind(aggregate_df, full_df)
  }
}

write.csv(aggregate_df, "../aggregate_mse_data.csv")

