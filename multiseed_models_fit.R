# Clear the environment
rm(list = ls())

# Install and load required libraries if not already available
required_packages <- c(
  "dplyr", "tictoc", "caret", "ROCit", "Metrics", 
  "rfUtilities", "writexl", "glmnet", "randomForest", "fastDummies"
)

# Check for missing packages and install them
missing_packages <- setdiff(required_packages, installed.packages()[, "Package"])

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load required libraries
library(dplyr)
library(tictoc)
library(caret)
library(ROCit)
library(Metrics)
library(rfUtilities)
library(writexl)
library(glmnet)
library(randomForest)
library(fastDummies)

# Define the seed for reproducibility
seed_main <- 4658
seeds <- c(seed_main, runif(9, 1, 9999))
casp_threshold <- 0

# Source custom functions
source(file = 'stratified.R')

# Load the data
data <- read.csv("data.csv")

# Prepare data

no_norm_cols <- c('outcome_bool')
vars_to_dummy <- c('gender', 'marital_status', 'owner_occupier', 'current_job')

# Initialize variables
i <- 1
multi_seed_resume <- list()

# Loop through seeds
for (seed in seeds) {
  set.seed(seed)
  
  # Split data into training and test sets
  splitted <- data %>% stratified(c("age", "CASP_12"), .8, bothSets = TRUE)
  training_set <- splitted$SET1
  test_set <- splitted$SET2
  
  # Store IDs and outcomes
  training_id <- training_set$subject_id
  test_id <- test_set$subject_id
  training_set$subject_id <- NULL
  test_set$subject_id <- NULL
  training_y <- training_set$outcome_bool
  test_y <- test_set$outcome_bool
  
  # Normalize data
  norm_param <- preProcess(training_set[ ,!names(training_set) %in% no_norm_cols], method = "range", rangeBounds = c(0, 1))
  training_norm <- predict(norm_param, training_set[!names(training_set) %in% no_norm_cols])
  test_norm <- predict(norm_param, test_set[!names(test_set) %in% no_norm_cols])
  
  # Restore full dataframe after normalization
  training_norm <- cbind(training_norm, 'outcome_bool' = training_y)
  test_norm <- cbind(test_norm, 'outcome_bool' = test_y)
  
  # Create dummy variables
  training_norm_dummy <- fastDummies::dummy_cols(training_norm, cols = vars_to_dummy, remove_first_dummy = TRUE)
  test_norm_dummy <- fastDummies::dummy_cols(test_norm, cols = vars_to_dummy, remove_first_dummy = TRUE)
  
  # Models fit - logistic regression
  set.seed(seed)
  model_LR <- glm(outcome_bool ~ . , data = training_norm, family = "binomial", maxit = 100)
  
  # Evaluate logistic regression
  prob_train_LR <- predict(model_LR, training_norm, type = "response")
  ROCit_obj_train_LR <- rocit(score = prob_train_LR, class = training_norm$outcome_bool)
  rocit.df_train_LR <- data.frame(tpp = ROCit_obj_train_LR$TPR * 100, fpp = (1 - ROCit_obj_train_LR$FPR) * 100, tresholds = ROCit_obj_train_LR$Cutoff)
  min_diff_train_LR <- min(abs(rocit.df_train_LR$tpp - rocit.df_train_LR$fpp))
  LR_opt_threshold <- rocit.df_train_LR$tresholds[abs(rocit.df_train_LR$tpp - rocit.df_train_LR$fpp) == min_diff_train_LR]
  
  # Models fit - logistic regression with LASSO
  set.seed(seed)
  idx_outcome <- grep("outcome_bool", colnames(training_norm_dummy))
  cv.LASSO <- cv.glmnet(as.matrix(training_norm_dummy[, -idx_outcome]), training_norm_dummy$outcome_bool, alpha = 1, family = "binomial")
  model_LASSO <- glmnet(training_norm_dummy[, -idx_outcome], training_norm_dummy$outcome_bool, alpha = 1, family = "binomial", lambda = cv.LASSO$lambda.min)
  
  # Evaluate logistic regression with LASSO
  prob_train_LASSO <- predict(model_LASSO, as.matrix(training_norm_dummy[, -idx_outcome]), type = "response")
  ROCit_obj_train_LASSO <- rocit(score = prob_train_LASSO, class = training_norm_dummy$outcome_bool)
  rocit.df_train_LASSO <- data.frame(tpp = ROCit_obj_train_LASSO$TPR * 100, fpp = (1 - ROCit_obj_train_LASSO$FPR) * 100, tresholds = ROCit_obj_train_LASSO$Cutoff)
  min_diff_train_LASSO <- min(abs(rocit.df_train_LASSO$tpp - rocit.df_train_LASSO$fpp))
  LASSO_opt_threshold <- rocit.df_train_LASSO$tresholds[abs(rocit.df_train_LASSO$tpp - rocit.df_train_LASSO$fpp) == min_diff_train_LASSO]
  
  # Models fit - random forest
  set.seed(seed)
  control <- trainControl(method = 'cv', number = 10, search = 'grid', savePredictions = 'final')
  tunegrid <- expand.grid(.mtry = c(4:10))
  model_RF <- train(outcome_bool ~ ., data = training_norm, method = 'rf', metric = 'Accuracy', tuneGrid = tunegrid, trControl = control, ntree = 500)
  
  # Performances LR
  prob_test_LR <- predict(model_LR, test_norm, type = "response")
  y_LR_train <- as.numeric(ifelse(prob_train_LR > LR_opt_threshold, "1", "0"))
  y_LR_test <- as.numeric(ifelse(prob_test_LR > LR_opt_threshold, "1", "0"))
  performances_LR_train <- accuracy(y_LR_train, training_norm$outcome_bool)
  performances_LR_test <- accuracy(y_LR_test, test_norm$outcome_bool)
  LR_accuracy_train_test = c(performances_LR_train$PCC, performances_LR_test$PCC)
  LR_AUC_train_test = c(performances_LR_train$auc, performances_LR_test$auc)
  LR_F1_train_test = c(performances_LR_train$f.score, performances_LR_test$f.score)
  
  # Performances LASSO
  prob_test_LASSO <- predict(model_LASSO, as.matrix(test_norm_dummy[, -idx_outcome]), type = "response")
  y_LASSO_train <- as.numeric(ifelse(prob_train_LASSO > LASSO_opt_threshold, "1", "0"))
  y_LASSO_test <- as.numeric(ifelse(prob_test_LASSO > LASSO_opt_threshold, "1", "0"))
  performances_LASSO_train <- accuracy(y_LASSO_train, training_norm$outcome_bool)
  performances_LASSO_test <- accuracy(y_LASSO_test, test_norm$outcome_bool)
  LASSO_accuracy_train_test = c(performances_LASSO_train$PCC, performances_LASSO_test$PCC)
  LASSO_AUC_train_test = c(performances_LASSO_train$auc, performances_LASSO_test$auc)
  LASSO_F1_train_test = c(performances_LASSO_train$f.score, performances_LASSO_test$f.score)
  
  # Performances RF
  best_mtry <- model_RF$bestTune$mtry
  pred_mtry <- model_RF$pred[model_RF$pred$mtry == best_mtry, ]
  y_RF_train <- predict(model_RF, training_norm)
  y_RF_test <- predict(model_RF, test_norm)
  prob_RF_test <- predict(model_RF, test_norm, type = "prob")
  performances_RF_train <- accuracy(y_RF_train, training_norm$outcome_bool)
  performances_RF_test <- accuracy(y_RF_test, test_norm$outcome_bool)
  RF_accuracy_train_test = c(model_RF$results$Accuracy[model_RF$results$mtry == best_mtry], performances_RF_test$PCC)
  RF_AUC_train_test = c(performances_RF_train$auc, performances_RF_test$auc)
  RF_F1_train_test = c(performances_RF_train$f.score, performances_RF_test$f.score)
  
  # Coefficients LR
  coeff_LR <- round(as.data.frame(summary(model_LR)[["coefficients"]]), digits = 3)
  
  # Coefficients LASSO
  coeff_LASSO <- round(as.data.frame(as.matrix(coef(model_LASSO))), digits = 3)
  
  if (seed == seed_main) {
    openxlsx::write.xlsx(coeff_LR, "output/coeff_LR_main.xlsx", rowNames = TRUE)
    openxlsx::write.xlsx(coeff_LASSO, "output/coeff_LASSO_main.xlsx", rowNames = TRUE)
  }
  
  # Store results
  this_seed <- list(
    training_id = training_id,
    test_id = test_id,
    LR_coefficients = coeff_LR,
    LR_predictions_train = y_LR_train,
    LR_predictions_test = y_LR_test,
    LR_accuracy = LR_accuracy_train_test,
    LR_AUC = LR_AUC_train_test,
    LR_F1 = LR_F1_train_test,
    LASSO_coefficients = coeff_LASSO,
    LASSO_predictions_train = y_LASSO_train,
    LASSO_predictions_test = y_LASSO_test,
    LASSO_accuracy = LASSO_accuracy_train_test,
    LASSO_AUC = LASSO_AUC_train_test,
    LASSO_F1 = LASSO_F1_train_test,
    best_m = best_mtry,
    RF_predictions_train = y_RF_train,
    RF_predictions_test = y_RF_test,
    RF_accuracy = RF_accuracy_train_test,
    RF_AUC = RF_AUC_train_test,
    RF_F1 = RF_F1_train_test,
    seed = seed
  )
  
  multi_seed_resume[[i]] <- this_seed
  
  # Update counter for storage
  i = i + 1
}

# Save results
saveRDS(multi_seed_resume, file = 'models_multiSeedResume.RDS')
