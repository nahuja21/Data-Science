
# clear workspace and set directory
rm(list = ls())
setwd("/Users/nikhilahuja/Documents/MGSC_Courses/MGSC_310/Assignment2/")

# load libraries
library(corrplot)
library(car)
library(lmtest)

# read data
data_raw <- read.csv("zencap@nahuja.csv")

# clean data no mission value errors
data <- data_raw[complete.cases(data_raw), ]

# visualzation
hist(data$Business_Failure, main="Business Failure Distribution")

# review cat-variables
unique(data$Location_Type)

# correlation anaylsis
cor_matrix <- cor(data[, sapply(data, is.numeric)])
corrplot(cor_matrix, method = 'number')

# logistic model
LN1 <- glm(Business_Failure ~ . - Company_ID, data = data, family = binomial)

# prediction on the data
predictions_prob <- predict(LN1, newdata = data, type = "response")
predictions <- ifelse(predictions_prob > 0.5, 1, 0) 

# confusion matrix and evlauation
conf_matrix <- table(Predicted = predictions, Actual = data$Business_Failure)

#model accuracy
accuracy <- mean(predictions == data$Business_Failure) * 100
cat("Accuracy:", accuracy, "%\n")

#print everything here so it is neater
summary(data)
summary(LN1)
print(conf_matrix)

# Conclusion----------
# This logistic regression model was developed to predict startup business failure, and it achieved an accuracy of around 77%.
# From the model summary, features like Debt_Ratio, Age_of_Business, Innovation_Index, and Median_Work_Experience appear to be significant predictors. 
# For instance, as the Age_of_Business increases, the chances of success also seem to increase. 
# Conversely, startups with a higher Median_Work_Experience are more likely to fail, which is counterintuitive.
# From the confusion matrix, out of the startups predicted, 255 were correctly identified as unsuccesful and 390 were succesful.
# The model missclassified 100 startups as succesful when they weren't and 90 unsuccessful when they were.
# My recommendation is to focus on is financial Stability. Nothing is worse than a startup already in a lot of debt.
# As well as a company with employee with good work experience. Companies with a better median_work_experience tend to do better.
# Finally encourage startups to foster a culture of innovation. Given the significance of the Innovation Index, it’s evident that innovative companies 
# have a competitive edge.