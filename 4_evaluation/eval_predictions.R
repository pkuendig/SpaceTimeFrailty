library(ROCR)
library(scoringRules)
library(hmeasure)

################################################################################
eval_auc <- function(real_defaults, pred_probs){
  rocr <- prediction(pred_probs, real_defaults)
  auc <- performance(rocr, "auc")@y.values[[1]]
  aucpr <- performance(rocr, "aucpr")@y.values[[1]]
  return(c(auc, aucpr))
}

################################################################################
eval_binary_logloss <- function(real_defaults, pred_probs){
  -mean(real_defaults * log(pred_probs) + (1-real_defaults) * log(1-pred_probs))
}  

################################################################################
brier_score <- function(real_defaults, pred_probs){
  mean((pred_probs-real_defaults)^2)
}

################################################################################
h_measure <- function(real_defaults, pred_probs){
  as.numeric(HMeasure(real_defaults, pred_probs, severity.ratio = NA, )$metrics[c('H')])
}
  
################################################################################
exp_calib_err <- function(real_defaults, pred_probs, nbr, bins){
  # Expected calibration error
  probs_pred <- probs_true <- rep(NA,nbr)
  ece <- 0
  for (i in 1:nbr) {
    sel <- pred_probs<=bins[i+1] & pred_probs>bins[i]
    if (sum(sel)>0) {
      probs_pred[i] <- mean(pred_probs[sel], na.rm=TRUE)
      probs_true[i] <- mean(real_defaults[sel], na.rm=TRUE)
      ece <- ece + sum(sel) * abs(probs_pred[i]-probs_true[i])
    }
  }
  ece / length(real_defaults)
}

eval_calib_bins <- function(models, n_pred_years, pred_years, nbr){
  #Pool predictions of all models over all years
  pred_probs <- c()
  for(model in models){
    pred_data <- readRDS(paste0("./../3_training_and_prediction/", model, "/data/prediction_", model ,".rds"))
    for (y in 1:n_pred_years) {
      pred_y_data <- pred_data[[paste0("pred_year_", pred_years[y])]]
      pred_probs <- c(pred_probs, pred_y_data$pred_response_mean)
    }
  }
  hist(pred_probs, breaks = 100)
  return(bins <- c(0,quantile(pred_probs, probs = (1:nbr)/nbr, na.rm=TRUE)))
}

################################################################################
#Evaluate portfolio loss distributions
eval_portfolio <- function(pf_dist, real_pf_loss){
  
  nsim <- length(pf_dist)
  qs <- quantile(pf_dist, probs=c(0.025,0.5,0.95,0.975,0.99))
  mu <- mean(pf_dist)
  
  #quantile losses
  diff <- real_pf_loss - qs[3]
  dummy <- ifelse(diff<0,1,0)
  q0.95_loss <- (0.95-dummy)*diff
  diff <- real_pf_loss - qs[5]
  dummy <- ifelse(diff<0,1,0)
  q0.99_loss <- (0.99-dummy)*diff
  
  #CRPS
  crps <- scoringRules::crps_sample(y = real_pf_loss, dat = pf_dist)
  
  return(c(qs, q0.95_loss, q0.99_loss, crps, mu)) #q2.5, q50, q95, q97.5, q99, q95_loss, q99_loss, crps, mu
}

################################################################################
#Simulate portfolio loss distributions
sim_portfolio <- function(n_sim, model, real_pf_loss, pred_y_data, liabilities){
  draw_latent <- grepl("GPBoost", model, fixed = TRUE) || grepl("GPModel", model, fixed = TRUE)
  if(draw_latent){
   L <- t(chol(pred_y_data$pred_latent_cov))
  }
  pf_dist <- rep(NA,n_sim)
  t1 <- Sys.time()
  for (s in 1:n_sim) {
    if(draw_latent){
      #draw from latent posterior predictive distribution
      latent_b <- pred_y_data$pred_latent_mean_b + L %*% rnorm(nrow(L))
      latent_mu <- pred_y_data$pred_latent_FE + latent_b[pred_y_data$b_to_obs]
      pred_probs <- 1 / (1 + exp(-latent_mu))
    } else{
      pred_probs <- pred_y_data$pred_response_mean
    }
    pred_defaults <- as.numeric(runif(length(liabilities)) <= pred_probs)
    pf_dist[s] <- sum(pred_defaults * liabilities)
  }
  t2 <- Sys.time()
  cat("Time portfolio simulation:", round(difftime(t2, t1, units='mins'), digits=2), "\n")
  return(eval_portfolio(pf_dist, real_pf_loss))
}

################################################################################
set.seed(1)

nbr <- 20 #number of bins
n_sim <- 1e5
do_sim <- TRUE #Should time consuming portfolio simulation be done?

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
yearly_data <- readRDS("./../1_data_set/data/data_window_2022.rds")
X <- yearly_data$X
Y <- yearly_data$Y

models <- c("GPBoost",
            "GPModel_spatio_temporal",
            "GPModel_spatial",
            "Logistic_regression") 

pred_years <- 2008:2022
n_pred_years <- length(pred_years)
X$year <- as.numeric(format(X$date_versioning, "%Y"))

#Evaluate calibration bins
bins <- eval_calib_bins(models, n_pred_years, pred_years, nbr)

all_eval <- data.frame()
for(model in models){
  cols_eval <- c("model", "pred_year", "real_pf_loss", "auc", "aucpr", "binary_logloss",
                 "brier_score", "h_measure", "exp_calib_err",
                 "q2.5", "q50", "q95", "q97.5", "q99", "q95_loss", "q99_loss", "crps", "mu")
  model_eval <- as.data.frame(matrix(nrow = n_pred_years, ncol = length(cols_eval)))
  colnames(model_eval) <- cols_eval
  model_eval$model <- model
  
  pred_data <- readRDS(paste0("./../3_training_and_prediction/", model, "/data/prediction_", model ,".rds"))
  
  for (y in 1:n_pred_years) {
    pred_year <- pred_years[y]
    model_eval$pred_year[y] <- pred_years[y]
    cat("Evaluation for year:", pred_years[y],"--- model:", model, "\n")
    
    i_active <- X$year == pred_year
    real_defaults <- Y[i_active]
    liabilities <- X$current_upb[i_active]
    
    pred_y_data <- pred_data[[paste0("pred_year_", pred_year)]]
    
    #auc
    pred_probs <- pred_y_data$pred_response_mean
    model_eval[y,c("auc", "aucpr")] <- eval_auc(real_defaults, pred_probs)
    
    #binary-logLoss
    model_eval[y,c("binary_logloss")] <- eval_binary_logloss(real_defaults, pred_probs)
    
    #brier-score
    model_eval[y,c("brier_score")] <- brier_score(real_defaults, pred_probs)
    
    #H-measure
    model_eval[y,c("h_measure")] <- h_measure(real_defaults, pred_probs)
    
    #Expected calibration error
    model_eval[y,c("exp_calib_err")] <- exp_calib_err(real_defaults, pred_probs, nbr, bins) 
        
    #portfolio loss
    real_pf_loss <- sum(liabilities * real_defaults)
    model_eval$real_pf_loss[y] <- real_pf_loss
    
    if(do_sim){
      #q2.5, q50, q95, q97.5, q99, q95_loss, q99_loss, crps, mu
      model_eval[y,10:18] <- sim_portfolio(n_sim, model, real_pf_loss, pred_y_data, liabilities)  
    }
  }
  saveRDS(model_eval, paste0("./eval_", model,".rds"))
  all_eval <- rbind(all_eval, model_eval)
}
saveRDS(all_eval, "./eval_all_models.rds")
