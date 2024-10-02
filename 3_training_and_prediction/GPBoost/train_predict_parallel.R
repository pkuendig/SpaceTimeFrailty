library(lubridate)
library(gpboost)
library(stringr)

set.seed(1)

################################################################################
args = commandArgs(trailingOnly=TRUE)

if (length(args)!=1) {
  stop("Prediction year must be supplied.", call.=FALSE)
} else{
  pred_year <- as.integer(args)
}

val_year <- pred_year-1
cat("Prediction year: ", pred_year, "\n")
cat("Validaiton year: ", val_year, "\n")

################################################################################
predictions <- vector("list", 1)
names(predictions) <- paste0("pred_year_", pred_year)

par_cnames <- c("year", "model",
                "GP_var", "GP_range_time", "GP_range_space",
                "INIT_GP_var", "INIT_GP_range_time", "INIT_GP_range_space", 
                "final_neg_log_likelihood", "time_training")
parameters <- as.data.frame(matrix(nrow=1, ncol=length(par_cnames)))
colnames(parameters) <- par_cnames

###Data preparation#############################################################
opt_hyperpars <- readRDS(paste0("./../../2_hyperparameter_tuning/data_", val_year,"/hyperpars_", val_year,".rds"))
vers_data <- readRDS(paste0("./../../1_data_set/data/data_window_", pred_year, ".rds"))

X <- vers_data$X
Y <- vers_data$Y

#Covariate: year of versioning
X$year_versioning <- as.numeric(format(X$date_versioning, "%Y"))
X$year_versioning[X$year_versioning == pred_year] <- pred_year - 1 #test year = last train year
X$year_versioning <- as.factor(X$year_versioning)  

#Design matrix
X_design <- model.matrix(~.,X[,!(names(X) %in% c("current_upb", "date_versioning"))],)
if(nrow(X) != nrow(X_design)) stop("X and X_design do not have the same number of rows.")

i_test <- as.numeric(format(X$date_versioning, "%Y")) == pred_year

#Train
X_train <- X_design[!i_test,]
coords_train <- cbind(time = as.numeric(as.character(X$year_versioning[!i_test])),
                      X[!i_test,c("X_centroid", "Y_centroid")])
Y_train <- Y[!i_test]
gpboost_dataset <- gpb.Dataset(data = X_train[,-1], label = Y_train)

#Test
X_test <- X_design[i_test,]
coords_test <- cbind(time = pred_year, 
                     X[i_test,c("X_centroid", "Y_centroid")])
Y_test <- Y[i_test]

cat("Predicting for year: ", pred_year, "\n")
cat("# train points: ", length(Y_train), "\n")
cat("# test points: ", length(Y_test), "\n")

###spatio-temporal GPBoost####################################################
gp_model <- GPModel(gp_coords = coords_train,
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

parameters$time_training[1] <- system.time(gpboost_model <- gpb.train(data = gpboost_dataset, 
                                                                      gp_model = gp_model,
                                                                      line_search_step_length = FALSE,
                                                                      num_leaves = 2^10,
                                                                      nrounds = opt_hyperpars$best_iter,
                                                                      learning_rate = opt_hyperpars$learning_rate,
                                                                      max_depth = opt_hyperpars$max_depth,
                                                                      min_data_in_leaf = opt_hyperpars$min_data_in_leaf, 
                                                                      lambda_l2 = opt_hyperpars$lambda_l2,
                                                                      verbose = 2))[3]

parameters$year[1] <- pred_year-1
parameters$model[1] <- "GPBoost"
parameters$GP_var[1] <- gp_model$get_cov_pars()[1]
parameters$GP_range_time[1] <- gp_model$get_cov_pars()[2]
parameters$GP_range_space[1] <- gp_model$get_cov_pars()[3]
parameters$INIT_GP_var[1] <- gp_model$get_optim_params()$init_cov_pars[1]
parameters$INIT_GP_range_time[1] <- gp_model$get_optim_params()$init_cov_pars[2]
parameters$INIT_GP_range_space[1] <- gp_model$get_optim_params()$init_cov_pars[3]
parameters$final_neg_log_likelihood[1] <- gp_model$get_current_neg_log_likelihood()

###Save unique training random effects for plotting###########################
i_unique <- which(!duplicated(coords_train))
train_coords_unique <- coords_train[i_unique,]
X_train_unique <- X_train[i_unique,]

train_latent <- predict(gpboost_model,
                        data = X_train_unique[,-1],
                        gp_coords_pred = as.matrix(train_coords_unique),
                        predict_var = FALSE,
                        predict_cov_mat = FALSE,
                        pred_latent = TRUE)

###Prediction#################################################################
pred_response <- predict(gpboost_model,
                         data = X_test[,-1],
                         gp_coords_pred = as.matrix(coords_test),
                         predict_var = FALSE,
                         predict_cov_mat = FALSE,
                         pred_latent = FALSE)

pred_latent <- predict(gpboost_model,
                       data = X_test[,-1],
                       gp_coords_pred = as.matrix(coords_test),
                       predict_var = FALSE,
                       predict_cov_mat = FALSE,
                       pred_latent = TRUE)

##latent covariance matrix of unique test locations
i_unique <- which(!duplicated(coords_test))
coords_test_unique <- coords_test[i_unique,]
X_test_unique <- X_test[i_unique,]

#Match latent b to observations
b_to_obs <- rep(NA, nrow(coords_test))
for(i in 1:nrow(coords_test)){
  b_to_obs[i] <- which(coords_test[i,"X_centroid"] == coords_test_unique[,"X_centroid"] &
                       coords_test[i,"Y_centroid"] == coords_test_unique[,"Y_centroid"] &
                       coords_test[i,"time"] == coords_test_unique[,"time"])
}
if(any(is.na(b_to_obs))) stop("NA's in match b to observation")
if(!all(coords_test_unique[b_to_obs,]==coords_test)) stop("latent match not fulfilled")

pred_latent_unique <- predict(gpboost_model,
                             data = X_test_unique[,-1],
                             gp_coords_pred = as.matrix(coords_test_unique),
                             predict_var = FALSE,
                             predict_cov_mat = TRUE,
                             pred_latent = TRUE)

predictions[[1]] <- list(pred_response_mean = pred_response$response_mean,
                         pred_latent_FE = pred_latent$fixed_effect,
                         pred_latent_mean_b = pred_latent_unique$random_effect_mean, 
                         pred_latent_cov = pred_latent_unique$random_effect_cov,
                         pred_latent_coords = coords_test_unique,
                         b_to_obs = b_to_obs,
                         train_latent_mean_b = train_latent$random_effect_mean,
                         train_coords_unique = train_coords_unique)

################################################################################
saveRDS(predictions, paste0("./data/prediction_", pred_year,".rds"))
saveRDS(parameters, paste0("./data/parameters_", (pred_year-1),".rds"))
