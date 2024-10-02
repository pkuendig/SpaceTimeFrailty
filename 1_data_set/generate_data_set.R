library(lubridate)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

import_years <- 1999:2022

###import data###########################################################
perform_data <- orig_data <- data.frame()

for(y in 1:length(import_years)){
  cat("Importing year:", import_years[y], "\n")
  ###origination data
  y_orig_data <- read.table(paste0("./data/sample_orig_",import_years[y],".txt"), sep="|")
  colnames(y_orig_data) <- c("credit_score", "date_first_payment", "first_time_homebuyer", "date_maturity",
                           "MSA", "insurance_percent", "nr_units", "occupancy", "original_combined_loan_to_value", 
                           "original_dept_to_income", "original_upb", "original_loan_to_value", "original_interest_rate",
                           "channel", "prepayment_penalty", "amortization_type", "property_state",
                           "property_type", "postal_code", "loan_number", "loan_purpose", "loan_term",
                           "number_of_borrowers", "seller_name", "servicer_name", "super_conforming",
                           "prerelief_loan_number", "program_indicator", "relief_refinance",
                           "valuation_method", "interest_only", "mi_cancellation")
  
  #SELECT: Single-Family property + fixed-rate mortgage + 30 years duration
  y_orig_data <- y_orig_data[y_orig_data$property_type=="SF" & y_orig_data$amortization_type=="FRM" & y_orig_data$loan_term==360,]
  orig_data <- rbind(orig_data, y_orig_data)
  cat("Number of new morgages:", nrow(y_orig_data), "\n")
  ###performance data
  y_perform_data <- read.table(paste0("./data/sample_svcg_",import_years[y],".txt"), sep="|")
  colnames(y_perform_data) <- c("loan_number", "period", "current_upb", "current_loan_delinquency_status",
                              "loan_age", "remaining_months", "date_defect_settlement", "modified",
                              "zero_balance_code", "date_zero_balance", "current_interest_rate",
                              "current_deferred_UPB", "DDLPI", "mi_recoveries", "net_sale_proceeds",
                              "non_mi_recoveries", "expenses", "legal_costs", "maintenance_costs",
                              "taxes_and_insurance", "miscellaneous_expenses", "actual_loss",
                              "modification_cost", "step_modification", "deferred_payment_plan",
                              "estimated_loan_to_value", "zero_balance_removal_UPB",
                              "delinquent_interest", "delinquent_desaster", "assistance_code",
                              "month_modification_cost", "interest_bearing_UPB")
  
  y_perform_data <- y_perform_data[y_perform_data$loan_number %in% y_orig_data$loan_number,]
  perform_data <- rbind(perform_data, y_perform_data)
  cat("Number of new performance entries:", nrow(y_perform_data), "\n")
}

rm(y_orig_data, y_perform_data)

#performance data till the end of 2022
perform_data <- perform_data[perform_data$period<202301,]

###match longitude + latitude###################################################
postcode_data <- read.csv("./data/ZIP_centroids.csv", sep=",")

#Restrict to mainland
postcode_data <- postcode_data[postcode_data$X_centroid < -66 & postcode_data$X_centroid > -125 
                               & postcode_data$Y_centroid > 24 & postcode_data$Y_centroid < 50,]

#Remove ZIP3==1, since it is distributed all over the country
postcode_data <- postcode_data[-which(postcode_data$ZIP3 == 1),]

orig_data$ZIP3 <- as.numeric(substr(orig_data$postal_code,1,nchar(orig_data$postal_code)-2))
orig_data <- merge(x=orig_data, y=postcode_data[,c("ZIP3", "X_centroid", "Y_centroid")], by="ZIP3", all = FALSE)
perform_data <- perform_data[perform_data$loan_number %in% orig_data$loan_number,]

###covariate cleaning###########################################################
orig_data$relief_refinance[is.na(orig_data$relief_refinance)] <- 9
orig_data$relief_refinance[orig_data$relief_refinance==""] <- 0
orig_data$relief_refinance[orig_data$relief_refinance=="Y"] <- 1

orig_data$first_time_homebuyer[orig_data$first_time_homebuyer=="N"] <- 0
orig_data$first_time_homebuyer[orig_data$first_time_homebuyer=="Y"] <- 1

orig_data$MSA[!is.na(orig_data$MSA)] <- 1
orig_data$MSA[is.na(orig_data$MSA)] <- 0

orig_data$super_conforming[orig_data$super_conforming==""] <- 0
orig_data$super_conforming[orig_data$super_conforming=="Y"] <- 1

#reporting how many borrowers
orig_data$number_of_borrowers[orig_data$number_of_borrowers==3] <- 2
orig_data$number_of_borrowers[orig_data$number_of_borrowers==4] <- 2

###imputation###################################################################
#in general: mode for categorical and mean for continuous variables

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

i_impute <- orig_data$original_dept_to_income == 999
orig_data$original_dept_to_income[i_impute] <- mean(orig_data$original_dept_to_income[!i_impute])

i_impute <- orig_data$original_combined_loan_to_value == 999
orig_data$original_combined_loan_to_value[i_impute] <- mean(orig_data$original_combined_loan_to_value[!i_impute])

i_impute <- orig_data$original_loan_to_value == 999
orig_data$original_loan_to_value[i_impute] <- mean(orig_data$original_loan_to_value[!i_impute])

i_impute <- orig_data$credit_score == 9999
orig_data$credit_score[i_impute] <- mean(orig_data$credit_score[!i_impute])

i_impute <- orig_data$nr_units == 99
orig_data$nr_units[i_impute] <- Mode(orig_data$nr_units[!i_impute])

i_impute <- orig_data$first_time_homebuyer == 9
orig_data$first_time_homebuyer[i_impute] <- Mode(orig_data$first_time_homebuyer[!i_impute])

i_impute <- orig_data$channel == 9
orig_data$channel[i_impute] <- Mode(orig_data$channel[!i_impute])

i_impute <- orig_data$number_of_borrowers == 99
orig_data$number_of_borrowers[i_impute] <- Mode(orig_data$number_of_borrowers[!i_impute])

i_impute <- orig_data$insurance_percent == 999
orig_data$insurance_percent[i_impute] <- mean(orig_data$insurance_percent[!i_impute])

###create factors###############################################################
for(c in c("occupancy", "loan_purpose", "first_time_homebuyer", "nr_units",
           "MSA", "channel", "number_of_borrowers", "relief_refinance")){
  orig_data[,c] <- as.factor(orig_data[,c])
}

###loan elimination#############################################################
#We eliminate loans that don't have a performance recording in the first 3 months
orig_data <- orig_data[orig_data$loan_number %in% perform_data$loan_number[(perform_data$loan_age==0 | 
                                                                            perform_data$loan_age==1 | 
                                                                            perform_data$loan_age==2 | 
                                                                            perform_data$loan_age==3) & perform_data$modified ==""],]
perform_data <- perform_data[perform_data$loan_number %in% orig_data$loan_number,]

###date: start, default, zero-balance###########################################

#default date = first time 90 days delinquent
date_defaults <- aggregate(period ~ loan_number,
                            data = perform_data[perform_data$current_loan_delinquency_status >=3,],
                            FUN = min)
colnames(date_defaults) <- c("loan_number", "date_default")
orig_data <- merge(x=orig_data, y=date_defaults, by="loan_number", all.x = TRUE)

#date zero-balance
orig_data <- merge(x=orig_data, y=perform_data[!is.na(perform_data$date_zero_balance),c("loan_number", "date_zero_balance")], 
                   by="loan_number", all.x = TRUE)

#convert to date format: add first day of the month
orig_data$date_first_payment <- as.Date(paste0(orig_data$date_first_payment, "01"), "%Y%m%d")
orig_data$date_maturity <- as.Date(paste0(orig_data$date_maturity, "01"), "%Y%m%d")
orig_data$date_default <- as.Date(paste0(orig_data$date_default, "01"), "%Y%m%d")
orig_data$date_zero_balance <- as.Date(paste0(orig_data$date_zero_balance, "01"), "%Y%m%d")

orig_data$date_loan_start <- orig_data$date_first_payment %m-% months(1)
orig_data$date_loan_end <- as.Date(apply(orig_data[,c("date_default", "date_zero_balance", "date_maturity")], 1, 
                                         FUN = min, na.rm=TRUE))

perform_data$period <- as.Date(paste0(perform_data$period, "01"), "%Y%m%d")

###versioning###################################################################

versioning_start <- as.Date("2000-01-01")
prediction_lag <- years(1)
versioning_dates <- versioning_start %m+% (0:22* prediction_lag)

orig_data$original_value = orig_data$original_upb / orig_data$original_loan_to_value * 100

#30-year fixed rate mortgage average in the united states from the Federal Reserve Bank of St. Louis
MORTGAGE30US <- read.csv("./data/MORTGAGE30US.csv", sep=",")
MORTGAGE30US$DATE <- as.Date(MORTGAGE30US$DATE)

X <- data.frame()
Y <- c()
ID <- c()
for(i in 1:length(versioning_dates)){
  current_date <- versioning_dates[i]
  prediction_cutoff <- current_date %m+% prediction_lag
  
  #active loans
  i_orig_active <- (orig_data$date_loan_start < current_date) & (orig_data$date_loan_end >= current_date)
  
  cat("Date: ", as.character(current_date), " | #active loans: ", sum(i_orig_active), "\n")
  
  #response: defaulting in [current_date, current_date + prediction_lag)
  Y_active <- as.numeric(orig_data$date_default[i_orig_active] < prediction_cutoff)
  Y_active[is.na(Y_active)] <- 0
  
  #id: loan-ID
  ID_active <- orig_data$loan_number[i_orig_active]
  
  ##time-independent covariates
  #"channel", "relief_refinance" - not included, since not all levels in all training periods
  X_active <- orig_data[i_orig_active, c("X_centroid", "Y_centroid", "credit_score", "occupancy", "nr_units", "loan_purpose",
                                         "first_time_homebuyer", "MSA", "insurance_percent", "original_dept_to_income", "original_combined_loan_to_value", "original_upb",
                                         "number_of_borrowers")]
  
  ##time-dependent covariates
  #versioning date
  X_active$date_versioning <- current_date
 
  #select latest perform entry for each active loan
  latest_perform <- perform_data[(perform_data$period < current_date) & 
                                   (perform_data$loan_number %in% orig_data$loan_number[i_orig_active]), c("loan_number", "period", "current_upb", "current_interest_rate")] %>% group_by(loan_number) %>% slice_max(period)
  #very new loans do not have yet a perform entry - take origination information instead
  latest_perform_new <- orig_data[i_orig_active & !(orig_data$loan_number %in% latest_perform$loan_number),c("loan_number", "date_loan_start", "original_upb", "original_interest_rate")]
  colnames(latest_perform_new) <- c("loan_number", "period", "current_upb", "current_interest_rate")
  latest_perform <- rbind(latest_perform, latest_perform_new)
  latest_perform <- latest_perform[match(orig_data$loan_number[i_orig_active], latest_perform$loan_number),]
  
  #current loan to value
  X_active$current_loan_to_value <- latest_perform$current_upb / orig_data$original_value[i_orig_active] * 100
  
  #current interest rate spread = current_interest_rate - MORGAGE30US
  X_active$ir_spread <- latest_perform$current_interest_rate - as.numeric((MORTGAGE30US %>% filter(DATE < current_date) %>% filter(DATE == max(DATE)))$MORTGAGE30US)
  
  #number of months from morgage origination
  X_active$n_months <- interval(start=orig_data$date_loan_start[i_orig_active], end=current_date) %/% months(1) - 1
  
  #to calculate portfolio-loss
  X_active$current_upb <- latest_perform$current_upb
  
  #remove active loan if latest perform entry is more than 6 months ago
  not_uptodate <- latest_perform$period < current_date %m-% months(6)
  X_active <- X_active[!not_uptodate,]
  Y_active <- Y_active[!not_uptodate]
  ID_active <- ID_active[!not_uptodate]
  
  X <- rbind(X, X_active)
  Y <- c(Y, Y_active)
  ID <- c(ID, ID_active)
  
  saveRDS(list(Y=Y, X=X, ID=ID), paste0("./data/data_window_", format(current_date,"%Y"),".rds"))
}
