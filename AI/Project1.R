#setting working directory
setwd("C:\\Users\\Project1")

getwd()

#loading required packages
library(dplyr)
library(ggplot2)
library(tidyr)

#Loading data into Rstudio
Sys.setlocale("LC_ALL", "English")
df <- read.csv("AB_NYC_2019.csv")

#summary
summary(df)
glimpse(df)

#checking for NA
summary(is.na(df))

#################################First Model Building Process#############################################

#First Data Splitting -> 70% training 30% test
set.seed(12345)
df <- df %>% mutate(id = row_number())
df_train <- df %>% sample_frac(.7) %>% filter(price > 0)
df_test  <- anti_join(df, df_train, by = 'id') %>% filter(price > 0)

# sanity check
nrow(df_train) + nrow(df_test) == nrow(df %>% filter(price > 0))

head(df_train,10)

#building model
first_model <- lm(price ~ neighbourhood_group + latitude + longitude + room_type + minimum_nights  + 
                         calculated_host_listings_count +
                          availability_365, data = df_train)
summary(first_model)

#######################################Data Exploration###################################################

#building histogram for checking price distribution
hist(df$price, main = "NYC Airbnb Price Distribution", xlab = "Price")

ggplot(df, aes(x = price)) +
  geom_histogram(aes(y = ..count..),binwidth=30)

ggplot(df, aes(y=price)) + 
  geom_boxplot(outlier.colour="red",notch=TRUE)

#price data cleaning
df_priceadj <- df %>% 
  filter(price < quantile(df$price, 0.9) & price > quantile(df$price, 0.1)) %>% 
  drop_na()

#################################Second Model Building Process#############################################

#Second Data Splitting -> 70% training 30% test
set.seed(12345)
df_priceadj <- df_priceadj %>% mutate(id = row_number())
df_priceadj_train <- df_priceadj %>% sample_frac(.7) %>% filter(price > 0)
df_priceadj_test  <- anti_join(df_priceadj, df_priceadj_train, by = 'id') %>% filter(price > 0)

# sanity check
nrow(df_priceadj_train) + nrow(df_priceadj_test) == nrow(df_priceadj %>% filter(price > 0))

#building model 2
second_model <- lm(price ~ neighbourhood_group + latitude + longitude + room_type + minimum_nights  + 
                    calculated_host_listings_count +
                    availability_365, data = df_priceadj_train)

second_model_sum <- summary(second_model)

second_model_sum

#Diagnostic Plots

par(mfrow = c(2,2))

plot(second_model)

#################################Cross Validation#############################################
#train error for final model#
mean(second_model_sum$residuals^2)

#test error for test model#
testmodel <- predict(object = second_model, newdata = df_priceadj_test)
mean((testmodel - df_priceadj_test$price)^2)





testPred <- predict(second_model, newdata = df_priceadj_test)
rmse <- sqrt(sum((exp(testPred) - df_priceadj_test$price)^2)/length(df_priceadj_test$price))
c(RMSE = rmse, R2 = summary(second_model)$r.squared)

par(mfrow=c(1,1))
plot(df_priceadj_test$price, exp(testPred))


original = df_priceadj_test$price
predicted = testPred
d = original-predicted
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
R2 = 1 -(sum((d)^2)/sum((original - mean(original))^2))
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-Sqaured:", R2)


# install.packages("caret")
library(caret)
predictions <- second_model %>% predict(df_priceadj_test)
RMSE(predictions, df_priceadj_test$price)
R2(predictions, df_priceadj_test$price)
cat("RMSE =", RMSE(predictions, df_priceadj_test$price), "\n", 
    "R2 =", R2(predictions, df_priceadj_test$price), "\n",
    "Error Rate =", RMSE(predictions, df_priceadj_test$price)/mean(df_priceadj_test$price))

mean(df_priceadj_test$price)
par(mfrow=c(1,1))
plot(df_priceadj_test$price, predictions, main="Predicted vs actual prices on test dataset", 
     xlab = "Actual price", ylab = "Predicted price", ylim=c(0,250))



#compare MSE
cat("In sample model - MSE =",mean(second_model_sum$residuals^2))
cat("Out sample model - MSE =",mean((testmodel - df_priceadj_test$price)^2))

#select columns only used in the model
df_select <- df %>% select(price, # place target variable first
                    #id,
                    #name,                          
                    #host_id,
                    #host_name,
                    neighbourhood_group,
                    #neighbourhood,                 
                    latitude, 
                    longitude,                     
                    room_type,                                               
                    minimum_nights, 
                    #number_of_reviews,             
                    #last_review, 
                    #reviews_per_month,             
                    calculated_host_listings_count,
                    availability_365)
head(df_select, 5)
