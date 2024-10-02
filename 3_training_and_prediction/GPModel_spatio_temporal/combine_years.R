setwd(dirname(rstudioapi::getSourceEditorContext()$path))

pred_years <- 2008:2022
n_pred_years <- length(pred_years) 

pred_cnames <- paste0("pred_year_", pred_years)
predictions <- vector("list", length(pred_cnames))
names(predictions) <- pred_cnames

par_cnames <- c("year", "model",
                "GP_var", "GP_range_time", "GP_range_space",
                "INIT_GP_var", "INIT_GP_range_time", "INIT_GP_range_space", 
                "final_neg_log_likelihood", "num_optim_iter",
                "time_training", paste0("beta_",0:41))
parameters <- as.data.frame(matrix(nrow=n_pred_years, ncol=length(par_cnames)))
colnames(parameters) <- par_cnames

for(i in 1:n_pred_years){
  
  y_parameters <- readRDS(paste0("./data/parameters_", (pred_years[i]-1), ".rds"))
  parameters[i,] <- y_parameters
  
  y_prediction <- readRDS(paste0("./data/prediction_", pred_years[i], ".rds"))
  predictions[i] <- y_prediction
}

saveRDS(parameters, "./data/parameters_GPModel_spatio_temporal.rds")
saveRDS(predictions, "./data/prediction_GPModel_spatio_temporal.rds")