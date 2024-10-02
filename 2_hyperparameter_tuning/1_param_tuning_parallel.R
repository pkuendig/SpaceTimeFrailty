library(lubridate)
library(gpboost)
library(stringr)

set.seed(1)

################################################################################
args = commandArgs(trailingOnly=TRUE)

if (length(args)!=2) {
  stop("Validation year and index of parameter combination must be supplied.", call.=FALSE)
} else{
  val_year <- as.integer(args[1])
  i_param <- as.integer(args[2])
}

cat("Validation year:", val_year, "\n")
cat("Index of parameter combination:", i_param, "\n")

################################################################################
##Hyperparameter combination
source("./gpb.cv.R")

full_param_grid = list("learning_rate" = c(10,1,0.1),
                        "min_data_in_leaf" = c(10,100,1000),
                        "max_depth" = c(2,3,5,10),
                        "lambda_l2" = c(0,1,10))

other_params <- list(num_leaves = 2^10)

#get.grid.size(full_param_grid)
param_grid <- get.param.combination(i_param, full_param_grid)

cat("learning_rate:", param_grid$learning_rate, "\n")
cat("min_data_in_leaf:", param_grid$min_data_in_leaf, "\n")
cat("max_depth:", param_grid$max_depth, "\n")
cat("lambda_l2:", param_grid$lambda_l2, "\n")

################################################################################
##Data preparation

vers_data <- readRDS(paste0("./../1_data_set/data/data_window_", val_year, ".rds"))

X <- vers_data$X
Y <- vers_data$Y

#Covariate: year of versioning
X$year_versioning <- as.numeric(format(X$date_versioning, "%Y"))
X$year_versioning[X$year_versioning == val_year] <- val_year - 1 #validation year = last train year
X$year_versioning <- as.factor(X$year_versioning)  

#Design matrix  
X_design <- model.matrix(~.,X[,!(names(X) %in% c("current_upb", "date_versioning"))],)
if(nrow(X) != nrow(X_design)) stop("X and X_design do not have the same number of rows.")

i_val <- as.numeric(format(X$date_versioning, "%Y")) == val_year

#Train
coords_train <- cbind(time = as.numeric(as.character(X$year_versioning[!i_val])),
                      X[!i_val,c("X_centroid", "Y_centroid")])

#Validation
coords_val <- cbind(time = val_year, 
                    X[i_val,c("X_centroid", "Y_centroid")])

coords <- rbind(coords_train,
                coords_val)

boost_dataset <- gpb.Dataset(data = X_design[,-1], label = Y)

folds <- list(which(i_val))

################################################################################
##Evaluation

param_comb <- as.data.frame(matrix(nrow=1, ncol=9))
colnames(param_comb) <- c("year", "model", "learning_rate", "min_data_in_leaf", 
                          "max_depth", "lambda_l2", "best_iter", "best_score", 
                          "time_grid_search")

gp_model <- GPModel(gp_coords = coords,
                    cov_function = "matern_space_time",
                    cov_fct_shape=1.5,
                    likelihood="bernoulli_logit",
                    gp_approx = "vecchia",
                    vecchia_ordering = "time_random_space",
                    num_neighbors = 20L,
                    matrix_inversion_method = "iterative")

gp_model$set_prediction_data(vecchia_pred_type = "latent_order_obs_first_cond_obs_only",
                             num_neighbors_pred = 20L)

if(val_year == 2007){
  gp_model$set_optim_params(list(trace=TRUE,
                                 init_cov_pars=c(0.8,2,9),
                                 seed_rand_vec_trace=1))
}else{
  gp_model$set_optim_params(list(trace=TRUE,
                                 seed_rand_vec_trace=1))
}

time_grid_search <- system.time({
  opt_params_gpboost <- gpb.grid.search.tune.parameters(param_grid = param_grid,
                                                        params = other_params,
                                                        line_search_step_length = FALSE,
                                                        num_try_random = NULL,
                                                        train_gp_model_cov_pars = TRUE,
                                                        folds = folds,
                                                        data = boost_dataset,
                                                        verbose_eval = 2,
                                                        gp_model = gp_model,
                                                        nrounds = 1000,
                                                        early_stopping_rounds = 20,
                                                        eval ="auc")})[3]

param_comb$year <- val_year
param_comb$model <- "GPBoost"
param_comb$learning_rate <- opt_params_gpboost$best_params$learning_rate
param_comb$min_data_in_leaf <- opt_params_gpboost$best_params$min_data_in_leaf
param_comb$max_depth <- opt_params_gpboost$best_params$max_depth
param_comb$lambda_l2 <- opt_params_gpboost$best_params$lambda_l2
param_comb$best_iter <- opt_params_gpboost$best_iter
param_comb$best_score <- opt_params_gpboost$best_score
param_comb$time_grid_search <- time_grid_search

saveRDS(param_comb, paste0("./data_", val_year ,"/param_comb_", i_param, ".rds"))
