#shows working directory
#setwd("")

getwd()
library(ggplot2)
library(dplyr)
library(Amelia)
library(caTools)
library(tidyverse)

ctmchurn_df <- read.table('TelcoCustomerChurnDataset.csv',sep=",", header=TRUE)

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
hist(ctmchurn_df$TotalCharges, breaks = 20)

#------------------------------------------------------Classification Model---------------------------------------------------------------

#reading data
df <- read.table('TelcoCustomerChurnDataset.csv',sep=",", header=TRUE)
head(df)
colnames(df)

#visualize structure of the data
str(df)
summary(df)

#change chr/int datatypes into factors
df$customerID<-as.factor(df$customerID)
df$gender<-as.factor(df$gender)
df$SeniorCitizen<-as.factor(df$SeniorCitizen)
df$Partner<-as.factor(df$Partner)
df$Dependents<-as.factor(df$Dependents)
df$PhoneService<-as.factor(df$PhoneService)
df$MultipleLines<-as.factor(df$MultipleLines)
df$InternetService<-as.factor(df$InternetService)
df$OnlineSecurity<-as.factor(df$OnlineSecurity)
df$OnlineBackup<-as.factor(df$OnlineBackup)
df$DeviceProtection<-as.factor(df$DeviceProtection)
df$TechSupport<-as.factor(df$TechSupport)
df$StreamingTV<-as.factor(df$StreamingTV)
df$StreamingMovies<-as.factor(df$StreamingMovies)
df$Contract<-as.factor(df$Contract)
df$PaperlessBilling<-as.factor(df$PaperlessBilling)
df$PaymentMethod<-as.factor(df$PaymentMethod)
df$Churn<-as.factor(df$Churn)

str(df)

#missing values
summary(is.na(df))

#delete ID columns
df <- df[-1]

# There are 11 cases with missing values in the TotalCharges variable.
dfemptyrows<- subset(df,is.na(df$TotalCharges))
dfemptyrows1<- df[is.na(df$TotalCharges),]

head(dfemptyrows)
#still loyalty customer without total charges.

sum(is.na(df$TotalCharges))/nrow(df)

# drop all rows with missing values
df_clean <- df[complete.cases(df), ]

df_clean$SeniorCitizen <- ifelse(df_clean$SeniorCitizen==0,"No","Yes")
df_clean$SeniorCitizen<-as.factor(df_clean$SeniorCitizen)

# the 'No phone service' response to 'No' for the MultipleLines variable.
df_clean$MultipleLines <- ifelse(df_clean$MultipleLines=="No phone service" | df_clean$MultipleLines=="No","No","Yes")
df_clean$OnlineSecurity <- ifelse(df_clean$OnlineSecurity=="No internet service" | df_clean$OnlineSecurity=="No","No","Yes")
df_clean$OnlineBackup <- ifelse(df_clean$OnlineBackup=="No internet service" | df_clean$OnlineBackup=="No","No","Yes")
df_clean$DeviceProtection <- ifelse(df_clean$DeviceProtection=="No internet service" | df_clean$DeviceProtection=="No","No","Yes")
df_clean$TechSupport <- ifelse(df_clean$TechSupport=="No internet service" | df_clean$TechSupport=="No","No","Yes")
df_clean$StreamingTV <- ifelse(df_clean$StreamingTV=="No internet service" | df_clean$StreamingTV=="No","No","Yes")
df_clean$StreamingMovies <- ifelse(df_clean$StreamingMovies=="No internet service" | df_clean$StreamingMovies=="No","No","Yes")

# change chr/int datatypes into factors
df_clean$gender<-as.factor(df_clean$gender)
df_clean$SeniorCitizen<-as.factor(df_clean$SeniorCitizen)
df_clean$Partner<-as.factor(df_clean$Partner)
df_clean$Dependents<-as.factor(df_clean$Dependents)
df_clean$PhoneService<-as.factor(df_clean$PhoneService)
df_clean$MultipleLines<-as.factor(df_clean$MultipleLines)
df_clean$InternetService<-as.factor(df_clean$InternetService)
df_clean$OnlineSecurity<-as.factor(df_clean$OnlineSecurity)
df_clean$OnlineBackup<-as.factor(df_clean$OnlineBackup)
df_clean$DeviceProtection<-as.factor(df_clean$DeviceProtection)
df_clean$TechSupport<-as.factor(df_clean$TechSupport)
df_clean$StreamingTV<-as.factor(df_clean$StreamingTV)
df_clean$StreamingMovies<-as.factor(df_clean$StreamingMovies)
df_clean$Contract<-as.factor(df_clean$Contract)
df_clean$PaperlessBilling<-as.factor(df_clean$PaperlessBilling)
df_clean$PaymentMethod<-as.factor(df_clean$PaymentMethod)
df_clean$Churn<-as.factor(df_clean$Churn)

str(df_clean)
head(df_clean)

#outliers
options(repr.plot.width=10, repr.plot.height=7)
hist(df_clean$tenure,main="Tenure Histogram",col = "red",xlab="Tenure",ylab="Count") 
hist(df_clean$MonthlyCharges,main="Monthly Chargers Histogram",col = "green",xlab="Monthly_charges",ylab="Count")
hist(df_clean$TotalCharges,main="Total Chargers Histogram",col = "purple",xlab="Total_charges",ylab="Count")

#Split Data into train data (70%) and test data (30%)
# split data 70% training 30% validation
set.seed(123)
sample_size = floor(0.7*nrow(df_clean))

split_data <- sample(1:nrow(df_clean), size = sample_size)
training_data<- df_clean[split_data,]
testing_data<- df_clean[-split_data,]

#------------------------------------------------------Logistic regression Model---------------------------------------------------------------

logModel <- glm(training_data$Churn~., data=training_data, family=binomial(link="logit"))
summary(logModel)

library(MASS)
logModel_2 <- stepAIC(logModel,direction = "both")
summary(logModel_2)
formula(logModel_2)

#Check for multicollinearity:
library(car)
(vif_vars <- as.data.frame(vif(logModel_2)))

#Final Model
logModel_3 <- glm(training_data$Churn ~ Dependents + tenure + PhoneService + MultipleLines + 
                    InternetService + OnlineBackup + DeviceProtection + StreamingTV + 
                    StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
                    TotalCharges,data = training_data,family = "binomial") 
summary(logModel_3)

#AIC for model 1 is 4173.5.
#AIC for model 2 is 4164.5.
#AIC for model 3 is 4193.
#model 2 will be selected since it has the lowest AIC among these three models.

#Model Evaluation
lr_prob1 <- predict(logModel_2, testing_data, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"Yes","No")
table(Predicted = lr_pred1, Actual = testing_data$Churn)

lr_prob2 <- predict(logModel_2, training_data, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"Yes","No")
lr_tab1 <- table(Predicted = lr_pred2, Actual = training_data$Churn)
lr_tab2 <- table(Predicted = lr_pred1, Actual = testing_data$Churn)
lr_acc <- sum(diag(lr_tab2))/sum(lr_tab2)
lr_acc
# Accuracy = 0.8189573



# training data accuracy
lr_prob2 <- predict(logModel_2, training_data, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"Yes","No")
confusionMatrix(table(lr_pred2, training_data$Churn))

# Ttesting data accuracy
confusionMatrix(lr_tab2)

#------------------------------------------------------Decision Tree Model---------------------------------------------------------------

library(C50)
library(caret)
decisionModel <- C5.0(training_data[,1:19], 
                      training_data$Churn)
summary(decisionModel)
plot(decisionModel)
C5imp(decisionModel)
# Training data  accuracy
preds_training <- predict(decisionModel, training_data, type="class")
confusionMatrix(preds_training, training_data$Churn)

# Apply the decision tree model to the validation data
preds_testing <- predict(decisionModel, testing_data, type="class")
# confusion matrix (Use validation data to test the tree model)
confusionMatrix(preds_testing, testing_data$Churn)

#------------------------------------------------------Random Forest Model---------------------------------------------------------------
#install.packages("randomForest")
library(randomForest)
RF_model <- randomForest(training_data$Churn ~ .,training_data)
print(RF_model)
plot(RF_model)

t <- tuneRF(training_data[, -20], training_data[, 20], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)
set.seed(12345)
RF_model_2 <- randomForest(training_data$Churn ~., 
                           data = training_data, ntree = 200, 
                           mtry = 2, importance = TRUE, proximity = TRUE)
print(RF_model_2)

varImpPlot(RF_model_2, sort=T,
           main="Important features")
importance(RF_model_2)
pred_rf_new <- predict(RF_model_2, testing_data)
confusionMatrix(pred_rf_new, testing_data$Churn)

# Evaluation training data
pred_rf_training_model <- predict(RF_model,training_data,type = "class")
confusionMatrix(pred_rf_training_model,training_data$Churn)
# Evaluation testing data
pred_rf_testing_model <- predict(RF_model,testing_data)
confusionMatrix(pred_rf_testing_model,testing_data$Churn)






# ROC analysis
library(pROC)
roc_lm <- roc(testing_data$Churn,lr_prob1,plot=T, legacy.axes=T, percent=T, 
              xlab="1-Specificity(False Postitive %)",
              ylab="Sensitivity(True Postitive %)")

pred_dm <- predict(decisionModel,testing_data,type = 'prob')
roc_dm <- roc(testing_data$Churn,pred_dm[,2],plot=T, legacy.axes=T, percent=T,
              xlab="1-Specificity(False Postitive %)",
              ylab="Sensitivity(True Postitive %)",
              main = "ROC analysis for Logistic, Decision Tree, & RF Model")

predict_forest_model <- predict(RF_model_2,testing_data,type ="prob" )
roc_rfm <- roc(testing_data$Churn,predict_forest_model[,2],plot=T, legacy.axes=T, percent=T,
              xlab="1-Specificity(False Postitive %)",
              ylab="Sensitivity(True Postitive %)",
              main = "ROC analysis for Logistic, Decision Tree, & RF Model")


# to add to the same graph: add=TRUE
plot(roc_lm, col = "red", lty = 1, add = TRUE, print.auc=T, legacy.axes=T,
     main = "ROC analysis for Logistic, Decision Tree, & RF Model")
plot(roc_dm, col = "blue", lty = 1, add = TRUE,print.auc=T, 
     print.auc.y=40,legacy.axes=T)
plot(roc_rfm, col = "black", lty = 1, add = TRUE,print.auc=T, 
     print.auc.y=30,legacy.axes=T)
legend("bottomright", c('Logistic','Decision','R-Forest'),lty=c(1,1),lwd=c(2,2),col=c('red','blue',"black"))


######
