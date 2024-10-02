require(lubridate)
require(gpboost)
library(stringr)

################################################################################
set.seed(1)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
pred_years <- 2008:2022
n_pred_years <- length(pred_years)

pred_cnames <- paste0("pred_year_", pred_years)
predictions <- vector("list", length(pred_cnames))
names(predictions) <- pred_cnames

par_cnames <- c("year", "model",
                "time_training", paste0("beta_",0:41))
parameters <- as.data.frame(matrix(nrow=n_pred_years, ncol=length(par_cnames)))
colnames(parameters) <- par_cnames

################################################################################
for(y in 1:n_pred_years){
  
  ###Prepare data###############################################################
  vers_data <- readRDS(paste0("./../../1_data_set/data/data_window_", pred_years[y], ".rds"))
  X <- vers_data$X
  Y <- vers_data$Y
  
  #Covariate: year of versioning
  X$year_versioning <- as.numeric(format(X$date_versioning, "%Y"))
  X$year_versioning[X$year_versioning == pred_years[y]] <- pred_years[y] - 1 #test year = last train year
  X$year_versioning <- as.factor(X$year_versioning)  
  
  #Design matrix
  X_design <- model.matrix(~.,X[,!(names(X) %in% c("current_upb", "date_versioning"))],)
  if(nrow(X) != nrow(X_design)) stop("X and X_design do not have the same number of rows.")
  
  i_test <- as.numeric(format(X$date_versioning, "%Y")) == pred_years[y]
  
  #Train
  X_train <- X_design[!i_test,]
  Y_train <- Y[!i_test]
  
  #Test
  X_test <- X_design[i_test,]
  Y_test <- Y[i_test]
  
  cat("Predicting for year: ", pred_years[y], "\n")
  cat("# train points: ", length(Y_train), "\n")
  cat("# test points: ", length(Y_test), "\n")
  
  ###linear independent model###################################################
  parameters$time_training[y] <- system.time({
    glmLog <- glm(Y_train ~ -1 + ., family=binomial(link="logit"), data=as.data.frame(X_train))})[3]
  
  betas <- glmLog$coefficients
  
  parameters$year[y] <- pred_years[y] - 1
  parameters$model[y] <- "Logistic_regression"
  
  parameters[y,4:(length(betas)+3)] <- betas
  
  ###Prediction###################################################################
  predLog <- predict(glmLog, newdata=as.data.frame(X_test), type="response")
  predictions[[y]] <- list(pred_response_mean = predLog)
}

################################################################################
saveRDS(predictions, "./data/prediction_Logistic_regression.rds")
saveRDS(parameters, "./data/parameters_Logistic_regression.rds")
