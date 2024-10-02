setwd(dirname(rstudioapi::getSourceEditorContext()$path))

for(val_year in 2007:2021){
  files <- list.files(path = paste0("./data_", val_year),
                      pattern = "\\.rds$")
  
  param_combs <- data.frame()
  
  for(i in 1:length(files)){
    param_comb <- readRDS(paste0("./data_", val_year, "/", files[i]))
    param_combs <- rbind(param_combs, param_comb)
  }
  
  i_best <- which.max(param_combs$best_score)
  
  saveRDS(param_combs[i_best,], paste0("./data_", val_year, "/hyperpars_", val_year,".rds"))
}