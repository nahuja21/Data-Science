
#clear workspace and set directory----
rm(list = ls())
setwd("/Users/nikhilahuja/Documents/MGSC_Courses/MGSC_310/Assignment3/")

#installs----
#install.packages('leaps')
#install.packages('caret')
#install.packages('glmnet')
#install.packages('rpart')
#install.packages('rpart.plot')

#libraries----
library(leaps)
library(caret)
library(glmnet)
library(rpart)
library(rpart.plot)

#read data----
raw_data <- read.csv("shielder@nahuja.csv")

#cleaning----
data <- raw_data[complete.cases(raw_data),]

#set seed----
set.seed(123)

#80%/20% split----
trainIdx <- createDataPartition(data$TARGET, p = 0.8, list = FALSE)
dataTrain <- data[trainIdx, ]
dataValid <- data[-trainIdx, ]

#train using the training set----
tree1 <- rpart(TARGET ~ ., data = dataTrain, method = 'class', control = rpart.control(cp = 0.01))

#plot it----
rpart.plot(tree1)

#predictions----
predictions <- predict(tree1, dataValid, type = "class")

#get accuracy----
accuracy <- sum(predictions == dataValid$TARGET) / nrow(dataValid)
print(paste("Accuracy:", accuracy))

#confusion matrix----
confusionMatrix <- table(dataValid$TARGET, predictions)
print(confusionMatrix)

#primary factors influencing loan defaults----

#EXT_SOURCE_2: This variable is the most important in predicting loan defaults according to the model,
#with the highest importance score, that predicts the client's ability to repay loans.

#DAYS_BIRTH: The age of the client (calculated as the number of days since birth) also seems to play a significant role.
#This is intuitive as age can be correlated with financial stability and risk aversion.

#DAYS_EMPLOYED: The number of days the client has been employed suggests that job stability is a key factor. 
#Longer employment may indicate a more stable income, which affects the ability to service debts.



#scale everything besides target: AMT variable----
preproc <- preProcess(data[, !names(data) %in% c("AMT_INCOME_TOTAL")], method = c("center", "scale"))
data_scaled <- predict(preproc, data[, !names(data) %in% c("AMT_INCOME_TOTAL")])
data_scaled$AMT_INCOME_TOTAL <- data$AMT_INCOME_TOTAL  # Add the target variable back after scaling

#partition 80/20 data sets----
trainIndex <- createDataPartition(data_scaled$AMT_INCOME_TOTAL, p = 0.8, list = FALSE)
trainData <- data_scaled[trainIndex, ]
validData <- data_scaled[-trainIndex, ]

#prepare matrices for glmnet----
trainDataMatrix_X <- as.matrix(trainData[, !names(trainData) %in% c("AMT_INCOME_TOTAL")])
trainDataMatrix_Y <- trainData$AMT_INCOME_TOTAL
validDataMatrix_X <- as.matrix(validData[, !names(validData) %in% c("AMT_INCOME_TOTAL")])

#lasso model----
#convert factors to dummy variables
dummies <- dummyVars(" ~ .", data = data)
data_transformed <- predict(dummies, newdata = data)
#convert it back to a data frame
data_transformed <- as.data.frame(data_transformed)
#extract the target variable
Y <- data_transformed$AMT_INCOME_TOTAL
#remove the target variable from the data frame
X_transformed <- data_transformed[, setdiff(names(data_transformed), "AMT_INCOME_TOTAL")]
#standardize the scale
X_scaled <- scale(X_transformed)
#training and test sets
trainIndex <- createDataPartition(Y, p = 0.8, list = FALSE)
X_train <- X_scaled[trainIndex, ]
Y_train <- Y[trainIndex]
X_test <- X_scaled[-trainIndex, ]
Y_test <- Y[-trainIndex]
#cross validation fit
cv_fit <- cv.glmnet(X_train, Y_train, alpha = 1)
#extract the best lambda
best_lambda <- cv_fit$lambda.min
#fit the final model using the best lambda
lasso_model <- glmnet(X_train, Y_train, alpha = 1, lambda = best_lambda)
#predict on the test set
predictions <- predict(lasso_model, newx = X_test, s = best_lambda)
#output the best lambda, coefficients, and RMSE
list(best_lambda = best_lambda, coefficients = coef(lasso_model, s = best_lambda), rmse = rmse)

#ridge regression----
#cross validation
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
#define lambda grid
lambda_grid <- expand.grid(alpha = 0:1, lambda = seq(0.001, 0.1, by = 0.001))
#train model
ridge_model <- train(
  x = as.matrix(dataTrain[, !names(dataTrain) %in% "AMT_INCOME_TOTAL"]),
  y = dataTrain$AMT_INCOME_TOTAL,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = lambda_grid
)
#get the best lambda value
best_lambda <- ridge_model$bestTune$lambda
#use the best lambda to train the final model
final_ridge_model <- glmnet(
  as.matrix(dataTrain[, !names(dataTrain) %in% "AMT_INCOME_TOTAL"]),
  dataTrain$AMT_INCOME_TOTAL,
  alpha = 0, 
  lambda = best_lambda
)
#make predictions
ridge_predictions <- predict(final_ridge_model, as.matrix(dataValid[, !names(dataValid) %in% "AMT_INCOME_TOTAL"]))
#calculate RMSE
rmse_ridge <- sqrt(mean((ridge_predictions - dataValid$AMT_INCOME_TOTAL)^2))

#rmse for both models----
print(paste("RMSE for Ridge Regression:", rmse_ridge))
print(paste("RMSE for Lasso Regression:", rmse_lasso))

#coefficients for ridge----
ridge_coefficients <- coef(final_ridge_model, s = best_lambda)
ridge_coefficients_matrix <- as.matrix(ridge_coefficients)
print("Ridge Coefficients:")
print(ridge_coefficients_matrix)


#coefficients for lasso----
lasso_coef <- predict(lasso_model, type="coefficients", s = opt_lambda_lasso)[1:ncol(trainDataMatrix_X),,drop=FALSE]
print("Lasso Coefficients:")
print(lasso_coef)



#qualitative assessment----
#Variable Importance: EXT_SOURCE_2, DAYS_BIRTH, and DAYS_EMPLOYED are the most significant predictors in the decision tree model.
#They indicate that external credit ratings and the client's age and employment history are highly influential in predicting the target variable,
#which makes sense in the context of credit scoring.
#Since we are working with incomes that can range from 1 million - 50k the rmse is going to be high. 
#In this case our rmses are 78k for ridge regression and 74k for lasso regression.

#ridge regression coefficients
#AMT_CREDIT, AMT_ANNUITY, DAYS_BIRTH, DAYS_EMPLOYED: are continuous variables with small coefficients meaning they directly affect the outcome of income but in a smaller way.
#CNT_CHILDREN, HOUR_APPR_PROCESS_START, AMT_REQ_CREDIT_BUREAU_DAY: These have larger coefficients, indicating a more substantial effect on income.

#lasso regression coefficients
#NAME_CONTRACT_TYPE, CODE_GENDER, NAME_EDUCATION_TYPE: Only certain levels of these categorical variables have non-zero coefficients,
#indicating they have an effect on income prediction in the Lasso model.
#DAYS_EMPLOYED: Similar to the Ridge model, this has a negative effect on income.

#for a more in depth analysis or a buisness strategy more data would need to be collected as well as using other types of regression models.




