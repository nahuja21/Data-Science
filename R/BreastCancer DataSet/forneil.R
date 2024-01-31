# Load necessary libraries
library(dplyr)
library(tidyr)
library(randomForest)
library(Metrics)
library(caret)
library(xgboost)


#set dir----
rm(list = ls())
setwd("/Users/nikhilahuja/Documents/MGSC_Courses/MGSC_310/forneil/")

# Read the data
data <- read.csv('test.csv')
data <- read.csv('train.csv')

# Handling missing values
data <- data %>% na.omit()  # This removes rows with any NA values
# Or more specific cleaning as per your requirements

# Replace NA values in 'patient_age' and 'bmi' to avoid NA in the new feature
data$patient_age[is.na(data$patient_age)] <- median(data$patient_age, na.rm = TRUE)  # Replace NA with median or another appropriate value
data$bmi[is.na(data$bmi)] <- median(data$bmi, na.rm = TRUE)

# Create a new feature as a ratio of patient_age to bmi
data$new_feature <- data$patient_age / data$bmi

# Assuming 'metastatic_cancer_diagnosis_code' is your treatment-related variable
# Check if the column exists in your dataset
if("metastatic_cancer_diagnosis_code" %in% names(data)) {
  # Create a new feature by combining 'patient_age' and 'metastatic_cancer_diagnosis_code'
  data$age_treatment_interaction <- paste(data$patient_age, data$metastatic_cancer_diagnosis_code, sep="_")
} else {
  stop("Treatment-related column does not exist in the dataset")
}


# Normalize function (min-max scaling)
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Apply normalization to relevant columns from your dataset
data$normalized_income <- normalize(data$income_household_median)
data$normalized_home_ownership <- normalize(data$home_ownership)
data$normalized_education <- normalize(data$education_college_or_above)
data$normalized_labor <- normalize(data$labor_force_participation)
data$normalized_unemployment <- 1 - normalize(data$unemployment_rate)  # Inverting as higher unemployment should reduce the score
data$normalized_married <- normalize(data$married)

# Handle NAs in normalized columns
columns_to_normalize <- c("normalized_income", "normalized_home_ownership", "normalized_education", 
                          "normalized_labor", "normalized_unemployment", "normalized_married")
data[columns_to_normalize] <- lapply(data[columns_to_normalize], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Create the "Quality of Life" metric as an average of the normalized variables
data$quality_of_life <- rowMeans(data[, columns_to_normalize], na.rm = TRUE)

# View the first few rows to confirm the new feature
head(data$quality_of_life)

# Write the modified dataset to a new CSV file
write.csv(data, "updated_dataset.csv", row.names = FALSE)




# View the first few rows to confirm the new feature
head(data$age_treatment_interaction)

# Prepare the data for training
target_column <- 'treatment_pd'
features <- setdiff(names(data), target_column)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
training_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

# Train the Random Forest model
# Assuming you have already split data into train_data and test_data
rf_model <- randomForest(treatment_pd ~ ., data=train_data, ntree=250, nodesize=1)
predictions <- predict(rf_model, test_data)

# Evaluate the model
mae_value <- mae(test_data$treatment_pd, predictions)
mse_value <- mse(test_data$treatment_pd, predictions)
rmse_value <- rmse(test_data$treatment_pd, predictions)

# Print model summary
print(rf_model)

# Predicting on the test set
predictions <- predict(rf_model, test_data)

# Evaluate the model
mae_value <- mae(test_data[[target_column]], predictions)
mse_value <- mse(test_data[[target_column]], predictions)
rmse_value <- rmse(test_data[[target_column]], predictions)

cat("Mean Absolute Error (MAE):", mae_value, "\n")
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")

# Setup for cross-validation
control <- trainControl(method="cv", number=10)

# Tuning grid setup
# Assuming you want to tune the 'mtry' parameter (number of variables randomly sampled as candidates at each split)
# You can adjust the value of mtry as needed
tuneGrid <- expand.grid(mtry=5)

# Train the model with cross-validation
set.seed(123) # for reproducibility
cv_model <- train(
  treatment_pd ~ ., 
  data = data,  
  method = "rf",
  ntree = 200,  # You can adjust the number of trees
  tuneGrid = tuneGrid,
  trControl = control
)

# Print the model summary
print(cv_model)



# Prepare the data for XGBoost
# Ensure the feature names are consistent and in the same order for both training and testing sets
feature_names <- setdiff(names(data), target_column)

# Create DMatrix for training
train_matrix <- data.matrix(train_data[, feature_names])
dtrain <- xgb.DMatrix(data = train_matrix, label = train_data[[target_column]])

# Create DMatrix for testing
test_matrix <- data.matrix(test_data[, feature_names])
dtest <- xgb.DMatrix(data = test_matrix)

# Define XGBoost parameters
xgb_params <- list(
  booster = "gbtree",
  objective = "reg:linear",  # or "reg:logistic" for regression, "binary:logistic" for binary classification
  eta = 0.3,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.5,
  colsample_bytree = 0.5,
  nthread = 2
)

# Train the XGBoost model
xgb_model <- xgb.train(params = xgb_params, data = dtrain, nrounds = 100)

# Predicting on the test set
xgb_predictions <- predict(xgb_model, dtest)

# Evaluate the XGBoost model
xgb_mae <- mae(test_data[[target_column]], xgb_predictions)
xgb_mse <- mse(test_data[[target_column]], xgb_predictions)
xgb_rmse <- rmse(test_data[[target_column]], xgb_predictions)

# Print model summary and evaluation metrics
print(xgb_model)
cat("XGBoost Mean Absolute Error (MAE):", xgb_mae, "\n")
cat("XGBoost Mean Squared Error (MSE):", xgb_mse, "\n")
cat("XGBoost Root Mean Squared Error (RMSE):", xgb_rmse, "\n")


# Define the number of folds for the cross-validation and number of boosting rounds
n_folds <- 10
nrounds_cv <- 100

# Perform cross-validation
cv_model <- xgb.cv(
  params = xgb_params, 
  data = dtrain, 
  nfold = n_folds, 
  nrounds = nrounds_cv, 
  metrics = list("rmse"), 
  showsd = TRUE, 
  early_stopping_rounds = 10,
  verbose = TRUE
)

# Print the cross-validation results
print(cv_model)
