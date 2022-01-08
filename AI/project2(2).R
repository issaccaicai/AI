setwd('')
library(ggplot2)
library(dplyr)
library(Amelia)
library(caTools)
library(tidyverse)

ctmchurn_df <- read.table('TelcoCustomerChurnDataset.csv',sep=",", header=TRUE)
View(ctmchurn_df)

#-------------------------------------Data Clean & Exploration------------------------------------------------------------------------------------------------------------------
#drop customer id
ctmchurn_df <- ctmchurn_df[,-1]

# Find missing Data and drop any rows with null values
any(is.na(ctmchurn_df))
missmap(ctmchurn_df, main = 'Missing Map', col=c('white', 'black'), legend=F)
# find some missing data in total charges 

# drop null values with 0
ctmchurn_df <- na.omit(ctmchurn_df)

str(ctmchurn_df)

# Demographic info
# convert gender, whether they have partners and dependents 
ctmchurn_df$gender <- as.factor(ctmchurn_df$gender)
ctmchurn_df$Partner<- as.factor(ctmchurn_df$Partner)
ctmchurn_df$Dependents <- as.factor(ctmchurn_df$Dependents)

# Services signed up
# set a function that simply converts values in rows contains "No ..." into "No" 
edit_serviceCol <- function(service) {
  out <- service
  for (i in 1:length(service)){
    if (grepl('No', service) == T){
      out[i] <- 'No'
    } else {
      out[i]
    }
  }
  out <- as.factor(out)
  return(out)
}

# Convert the columns about Services
ctmchurn_df$PhoneService <- sapply(ctmchurn_df$PhoneService, edit_serviceCol)
ctmchurn_df$MultipleLines <- sapply(ctmchurn_df$MultipleLines, edit_serviceCol)
ctmchurn_df$InternetService <- sapply(ctmchurn_df$InternetService, edit_serviceCol)
ctmchurn_df$OnlineSecurity <- sapply(ctmchurn_df$OnlineSecurity, edit_serviceCol)
ctmchurn_df$OnlineBackup <- sapply(ctmchurn_df$OnlineBackup, edit_serviceCol)
ctmchurn_df$DeviceProtection <- sapply(ctmchurn_df$DeviceProtection, edit_serviceCol)
ctmchurn_df$TechSupport <- sapply(ctmchurn_df$TechSupport, edit_serviceCol)
ctmchurn_df$StreamingTV <- sapply(ctmchurn_df$StreamingTV, edit_serviceCol)
ctmchurn_df$StreamingMovies <- sapply(ctmchurn_df$StreamingMovies, edit_serviceCol)
str(ctmchurn_df)

# Account Info
ctmchurn_df$Contract <- as.factor(ctmchurn_df$Contract)
ctmchurn_df$PaperlessBilling <- as.factor(ctmchurn_df$PaperlessBilling)
ctmchurn_df$PaymentMethod <- as.factor(ctmchurn_df$PaymentMethod)
# Churn
ctmchurn_df$Churn <- as.factor(ctmchurn_df$Churn)

# After factorize some columns, visualize some numeric variables

# Demographic info viz
for (i in 1:4) {
  print(ggplot(ctmchurn_df, aes(x=ctmchurn_df[,i]))+
           geom_bar(fill='pink') +
          xlab(colnames(ctmchurn_df)[i]))
  Sys.sleep(2)
}
#hist(x=ctmchurn_df$tenure, breaks = 30)
ggplot(ctmchurn_df, aes(x=tenure))+ 
  geom_histogram(aes(y=..density..), color='blue',fill='white', bins=30) +
  geom_density(color='red') +
  xlim(0,80)+
  ggtitle("Histogram of tenure")

# the percentage of customers who have used Telco over 2 years
nrow(subset(ctmchurn_df, tenure >= 24))/ nrow(ctmchurn_df)

# the percentage of customers who have used Telco less than 1 year
nrow(subset(ctmchurn_df, tenure < 12))/ nrow(ctmchurn_df)


# Service viz
for (i in 6:14) {
  print(ggplot(ctmchurn_df, aes(x=ctmchurn_df[,i]))+
          geom_bar(fill='blue')+
          xlab(colnames(ctmchurn_df)[i]))
  Sys.sleep(2)
}

# barchart of tenure and internet service 
ggplot(ctmchurn_df, aes(x=tenure))+
  geom_bar(aes(fill=InternetService))

nrow(subset(ctmchurn_df, tenure < 12 & InternetService !='No'))/ nrow(subset(ctmchurn_df, tenure < 12))
nrow(subset(ctmchurn_df, tenure >= 24 & InternetService !='No'))/ nrow(subset(ctmchurn_df, tenure >= 24))


nrow(subset(ctmchurn_df, tenure < 12 & PhoneService !='No'))/ nrow(subset(ctmchurn_df, tenure < 12))
nrow(subset(ctmchurn_df, tenure >= 24 & PhoneService !='No'))/ nrow(subset(ctmchurn_df, tenure >= 24))

mean(ctmchurn_df$MonthlyCharges) *24

# Account info viz
for (i in 15:17) {
  print(ggplot(ctmchurn_df, aes(x=ctmchurn_df[,i]))+
          geom_bar(fill='orange')+
          xlab(colnames(ctmchurn_df)[i]))
  Sys.sleep(2)
}
hist(ctmchurn_df$MonthlyCharges, breaks = 20, labels=F, main='Histogram of Monthly Charges', xlab='Monthly charge')
hist(ctmchurn_df$TotalCharges, breaks = 20, labels=F, main='Histogram of Total Charges', xlab='Total charge')
?hist(ctmchurn_df$TotalCharges, breaks = 20)

#------------------------------------------------------Classfication Models---------------------------------------------------------------
# Set a random seed
set.seed(123)

# Split up the sample
sample <- sample.split(ctmchurn_df$Churn, SplitRatio = 0.70) 

# 70% Training Data
train_data_1 <- subset(ctmchurn_df, sample == TRUE)

# 30% Testing Data
test_data_1 <- subset(ctmchurn_df, sample == FALSE)

# Create a logit model
logit_model_1 <- glm(Churn ~ ., family = binomial(logit), data = train_data_1)
summary(logit_model_1)
# The p-values of SeniorCitizen, tenure, MultipleLines, InternetService, 
# InternetService,StreamingTV, Contract, PaperlessBilling, and TotalCharges are less than 0.05

# Use these features to re-build a logit model
train_data_2 <- subset(ctmchurn_df[,c(2,5,7,8,13,15,16,19,20)], sample == TRUE)
test_data_2 <- subset(ctmchurn_df[,c(2,5,7,8,13,15,16,19,20)], sample == FALSE)

logit_model_2 <- glm(Churn ~ SeniorCitizen + tenure + MultipleLines + InternetService +
                     InternetService + StreamingTV + Contract + PaperlessBilling + TotalCharges, family = binomial(logit), data = train_data_2)
summary(logit_model_2)
# After repeat the previous steps and get a new model, all of p-values of these features are less than 0.05 

# Fit test data in the model and make predictions
prob <- logit_model_2 %>% 
  predict(test_data_2, type='response')

# Set the prediction as Yes if probability is greater than 0.5
fitted_results <- ifelse(prob>0.5, 'Yes', 'No')
fitted_results <- factor(fitted_results)

misClassificError <- mean(fitted_results != test_data_2$Churn)

print(paste('Accuracy rate is',1-misClassificError))
# "Accuracy rate is 0.791292001893043"

# Now use decision tree algorithm to predict the probability of Customer Churn
library(C50)
library(caret)

# Adopt the previous training and testing datasets to build a decision tree
tree_Model <- C5.0(x=train_data_1[,c(1:(ncol(ctmchurn_df)-1))], y=train_data_1$Churn)
summary(tree_Model)

#plot decision tree
plot(tree_Model, subtree= 5)

#predict by the model
pred <- predict(tree_Model, test_data_1[,c(1:(ncol(ctmchurn_df)-1))])
summary(tree_Model) # Accuracy rate is 0.813

# Use validation data to test the tree model
confusionMatrix(pred, test_data_1$Churn)# Prediction Accuracy: 0.7866 



