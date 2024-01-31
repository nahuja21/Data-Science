#set dir----
rm(list = ls())
setwd("/Users/nikhilahuja/Documents/MGSC_Courses/MGSC_310/AAP/dataset/")

#libraries----
library(doParallel)
library(readr)
library(ggplot2)
library(corrplot)
library(randomForest)
library(caret)

#preprocessing----
#load data set
games <- read_csv("games.csv")

#summary
summary(games)

#check missing values
sapply(games, function(x) sum(is.na(x)))

#feature engineering
games$date_release <- as.Date(games$date_release)
games$release_year <- format(games$date_release, "%Y")


#first feature for platform
games$platform_count <- rowSums(games[, c("win", "mac", "linux")])

#next feature review
rating_levels <- c('Mostly Negative', 'Mixed', 'Positive', 'Very Positive', 'Overwhelmingly Positive')
games$rating <- factor(games$rating, levels = rating_levels, ordered = TRUE)
#make integers
games$rating_numeric <- as.integer(games$rating)
#test check
head(games)


#learn about variables----
#histogram
ggplot(games, aes(x = price_final)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Game Prices", x = "Price", y = "Count")

#scatter plot
ggplot(games, aes(x = user_reviews, y = price_final)) + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Price vs User Reviews", x = "User Reviews", y = "Price")

#correlation anaylsis
num_cols <- sapply(games, is.numeric) # select numeric columns for correlation
cor_matrix <- cor(games[, num_cols], use = "complete.obs")
corrplot(cor_matrix, method = 'number')


#linear model----
linear_model <- lm(price_final ~ user_reviews + discount, data = games)

#summary
summary(linear_model)

#diagnostic plots
par(mfrow=c(2,2))
plot(linear_model)


#new linear model----
enhanced_linear_model <- lm(price_final ~ user_reviews + discount + platform_count + rating_numeric, data = games)

#summary of new
summary(enhanced_linear_model)

#diagnostic plots
par(mfrow=c(2,2))
plot(enhanced_linear_model)


#randomforestation----
set.seed(123)
rf_model <- randomForest(price_final ~ . - title - date_release - app_id, data = games, na.action = na.omit)

#summary
print(rf_model)

#missing values again
games_no_na <- na.omit(games)

#correlation between the actual and predicted values
rf_r_squared <- cor(games_no_na$price_final, predict(rf_model))^2
print(rf_r_squared)


#cross-validation setup
control <- trainControl(method="cv", number=10)

#tuning grid using mtry
tuneGrid <- expand.grid(mtry=3)

#train with CV
set.seed(123) 
cv_model <- train(
  price_final ~ . - title - date_release - app_id, 
  data = games_no_na,  
  method = "rf",
  ntree = 100,  
  tuneGrid = tuneGrid,
  trControl = control
)

#print
print(cv_model)

#compatibility analysis----
#calc avg mean and count
games$compatibility <- paste(games$win, games$mac, games$linux, games$steam_deck, sep = "-")
platform_prices <- aggregate(price_final ~ compatibility, data = games, FUN = function(x) c(mean = mean(x), median = median(x), count = length(x)))

#convert into data frame
platform_prices_df <- do.call(data.frame, platform_prices)
names(platform_prices_df)[2:4] <- c("MeanPrice", "MedianPrice", "Count")

#order by mean price
platform_prices_ordered <- platform_prices_df[order(-platform_prices_df$MeanPrice), ]

#output
print(platform_prices_ordered)

