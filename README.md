# Spatio-Temporal Machine Learning for Mortgage Credit Risk 

This repository contains the R code to generate the data set and run the mortgage credit risk application in the paper "A Spatio-Temporal Machine Learning Model for Mortgage Credit Risk: Default Probabilities and Loan Portfolios". 
The linear spatial, linear spatio-temporal, and tree-boosted spatio-temporal frailty models are implemented in the GPBoost package, see <https://github.com/fabsig/GPBoost>. 

In general, the code is structured so that calculations in step 2 and 3 for different (validation/training/prediction) years and hyperparameter combinations can be run in parallel in different batch jobs on a computing environment.

## Step 1: Generating the data set

To generate the mortgage credit risk data set from the publicly available single family loan-level data set from Freddie Mac proceed as follows:

1. Download the standard annual sample data sets from 1999 to 2022 at https://www.freddiemac.com/research/datasets/sf-loanlevel-dataset. Store the extracted text files (`sample_orig_YYYY.txt`, `sample_svcg_YYYY.txt`) in the folder `root\1_data_set\data`.

2. Download the average interest rate issued by Freddie Mac for 30-year fixed-rate mortgages from the Federal Reserve Bank of St. Louis at https://fred.stlouisfed.org/series/MORTGAGE30US. Name the downloaded csv file as `MORTGAGE30US.csv` and save it in `root\1_data_set\data`.

3. Run `root\1_data_set\generate_data_set.R` to read the downloaded text files and to generate the mortgage credit risk data set.


## Step 2: Hyperparameter tuning

To conduct hyperparameter tuning for the tree-boosted spatio-temporal frailty model proceed as follows: 

1. Create the subfolders `data_2007`, `data_2008`, ..., `data_2021` in the directory `root\2_hyperparameter_tuning`.

2. Evaluate each hyperparameter combination (`comb_nr`, values: 1-108) for each validation year (`y_val`, values: 2007-2021) by running `Rscript param_tuning_parallel.R y_val comb_nr` in batch jobs on your computing environment.

3. Run `root\2_hyperparameter_tuning\2_select_best_param_comb.R` to select the best hyperparameter combination for each validation year.


## Step 3: Training and prediction

To conduct training and prediction for the different years with the linear independent (`root\3_training_and_prediction\Logistic_regression`),  linear spatial (`root\3_training_and_prediction\GPModel_spatial`), linear spatio-temporal (`root\3_training_and_prediction\GPModel_spatio_temporal`), and tree-boosted spatio-temporal frailty (`root\3_training_and_prediction\GPBoost`) models proceed as follows:

1. Navigate to the desired model directory and create a subfolder called `data`.

2. Perform training and prediction for each prediction year (`y_pred`, values: 2008-2022) by running `Rscript train_predict_parallel.R y_pred` in batch jobs on your computing environment. For the linear independent model, run `train_predict.R` once instead (no multiple batch jobs required).

3. Run `combine_years.R` to combine the predictions and parameter estimates for the different prediction / training years (not necessary for the linear independent model).


## Step 4: Evaluation

To evaluate the prediction accuracy for the different models and prediction years run `root\4_evaluation\eval_predictions.R`. Various prediction accuracy measures for predictive default probabilities are calculated and predictive loan portfolio loss distributions are approximated using simulations. 