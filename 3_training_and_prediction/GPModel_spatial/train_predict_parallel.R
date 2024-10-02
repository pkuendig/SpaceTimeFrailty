library(lubridate)
library(gpboost)
library(stringr)

################################################################################
args = commandArgs(trailingOnly=TRUE)

if (length(args)!=1) {
  stop("Prediction year must be supplied.", call.=FALSE)
} else{
  pred_year <- as.integer(args)
}
cat("Prediction year: ", pred_year, "\n")

###Data preparation#############################################################
set.seed(1)
predictions <- vector("list", 1)
names(predictions) <- paste0("pred_year_", pred_year)

par_cnames <- c("year", "model",
                "GP_var", "GP_range_space",
                "INIT_GP_var", "INIT_GP_range_space", 
                "final_neg_log_likelihood", "num_optim_iter",
                "time_training", paste0("beta_",0:41))
parameters <- as.data.frame(matrix(nrow=1, ncol=length(par_cnames)))
colnames(parameters) <- par_cnames

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
coords_train <- X[!i_test,c("X_centroid", "Y_centroid")]
Y_train <- Y[!i_test]

#Test
X_test <- X_design[i_test,]
coords_test <- X[i_test,c("X_centroid", "Y_centroid")]
Y_test <- Y[i_test]

cat("Predicting for year: ", pred_year, "\n")
cat("# train points: ", length(Y_train), "\n")
cat("# test points: ", length(Y_test), "\n")

###spatial GPModel##############################################################

gp_model <- GPModel(gp_coords = coords_train,
                    cov_function = "matern",
                    cov_fct_shape = 1.5,
                    likelihood="bernoulli_logit",
                    gp_approx = "vecchia",
                    vecchia_ordering = "random",
                    num_neighbors = 20L,
                    matrix_inversion_method = "iterative")

gp_model$set_prediction_data(vecchia_pred_type = "latent_order_obs_first_cond_obs_only",
                             num_neighbors_pred=20L)

parameters$time_training[1] <- system.time(gp_model$fit(y=Y_train, X=X_train))[3]
betas <- gp_model$get_coef()

parameters$year[1] <- pred_year - 1
parameters$model[1] <- "GPModel_spatial"

parameters$GP_var[1] <- gp_model$get_cov_pars()[1]
parameters$GP_range_space[1] <- gp_model$get_cov_pars()[2]

parameters$INIT_GP_var[1] <- gp_model$get_optim_params()$init_cov_pars[1]
parameters$INIT_GP_range_space[1] <- gp_model$get_optim_params()$init_cov_pars[2]

parameters$final_neg_log_likelihood[1] <- gp_model$get_current_neg_log_likelihood()
parameters$num_optim_iter[1] <- gp_model$get_num_optim_iter()

parameters[1,10:(length(betas)+9)] <- betas

###Save unique training random effects for plotting###########################
i_unique <- which(!duplicated(coords_train))
train_coords_unique <- coords_train[i_unique,]
X_train_unique <- X_train[i_unique,]

train_latent <- predict(gp_model,
                        X_pred = X_train_unique,
                        gp_coords_pred = as.matrix(train_coords_unique),
                        predict_var = FALSE,
                        predict_cov_mat = FALSE,
                        predict_response = FALSE)

train_latent_mean_b <- train_latent$mu - c(X_train_unique %*% betas)

###Prediction#################################################################
pred_response <- predict(gp_model,
                         X_pred = X_test,
                         gp_coords_pred = as.matrix(coords_test),
                         predict_var = FALSE,
                         predict_cov_mat = FALSE,
                         predict_response = TRUE)

##latent covariance matrix of unique test locations
i_unique <- which(!duplicated(coords_test))
coords_test_unique <- coords_test[i_unique,]
X_test_unique <- X_test[i_unique,]

#Match latent b to observations
b_to_obs <- rep(NA, nrow(coords_test))
for(i in 1:nrow(coords_test)){
  b_to_obs[i] <- which(coords_test[i,"X_centroid"] == coords_test_unique[,"X_centroid"] &
                       coords_test[i,"Y_centroid"] == coords_test_unique[,"Y_centroid"])
}
if(any(is.na(b_to_obs))) stop("NA's in match b to observation")
if(!all(coords_test_unique[b_to_obs,]==coords_test)) stop("latent match not fulfilled")

pred_latent <- predict(gp_model,
                       X_pred = X_test_unique,
                       gp_coords_pred = as.matrix(coords_test_unique),
                       predict_var = FALSE,
                       predict_cov_mat = TRUE,
                       predict_response = FALSE)

pred_latent_mean_b <- pred_latent$mu - c(X_test_unique %*% betas)

predictions[[1]] <- list(pred_response_mean = pred_response$mu,
                         pred_latent_FE = c(X_test %*% betas),
                         pred_latent_mean_b = pred_latent_mean_b, 
                         pred_latent_cov = pred_latent$cov,
                         pred_latent_coords = coords_test_unique,
                         b_to_obs = b_to_obs,
                         train_latent_mean_b = train_latent_mean_b,
                         train_coords_unique = train_coords_unique)

################################################################################
saveRDS(predictions, paste0("./data/prediction_", pred_year,".rds"))
saveRDS(parameters, paste0("./data/parameters_", (pred_year-1),".rds"))
