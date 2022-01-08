# read csv data file
projectdata<-read.table("TelcoCustomerChurnDataset.csv",sep=",",header=T)

colnames(projectdata)

# visualize structure of the data
str(projectdata)

# change chr/int datatypes into factors
projectdata$customerID<-as.factor(projectdata$customerID)
projectdata$gender<-as.factor(projectdata$gender)
projectdata$SeniorCitizen<-as.factor(projectdata$SeniorCitizen)
projectdata$Partner<-as.factor(projectdata$Partner)
projectdata$Dependents<-as.factor(projectdata$Dependents)
projectdata$PhoneService<-as.factor(projectdata$PhoneService)
projectdata$MultipleLines<-as.factor(projectdata$MultipleLines)
projectdata$InternetService<-as.factor(projectdata$InternetService)
projectdata$OnlineSecurity<-as.factor(projectdata$OnlineSecurity)
projectdata$OnlineBackup<-as.factor(projectdata$OnlineBackup)
projectdata$DeviceProtection<-as.factor(projectdata$DeviceProtection)
projectdata$TechSupport<-as.factor(projectdata$TechSupport)
projectdata$StreamingTV<-as.factor(projectdata$StreamingTV)
projectdata$StreamingMovies<-as.factor(projectdata$StreamingMovies)
projectdata$Contract<-as.factor(projectdata$Contract)
projectdata$PaperlessBilling<-as.factor(projectdata$PaperlessBilling)
projectdata$PaymentMethod<-as.factor(projectdata$PaymentMethod)
projectdata$Churn<-as.factor(projectdata$Churn)

# visualize the summary of the dataset
summary(projectdata)

# checking NA values
# Let's begin by seeing if there is any missing data.
summary(is.na(projectdata))
sapply(projectdata, function(x) sum(is.na(x)))

# delete ID columns
projectdata <- projectdata[-1]
# data with empty rows
# There are 11 cases with missing values in the TotalCharges variable.
# Let's see these particular cases.
projectemptyrows<- subset(projectdata,is.na(projectdata$TotalCharges))
projectemptyrows1<- projectdata[is.na(projectdata$TotalCharges),]


# Inspection of the Churn variable shows that these are all still subscribing 
# customers. What proportion of our sample is this subset with missing values?
sum(is.na(projectdata$TotalCharges))/nrow(projectdata)

cleanData <- projectdata[complete.cases(projectdata), ]

cleanData$SeniorCitizen <- ifelse(cleanData$SeniorCitizen==0,"No","Yes")
cleanData$SeniorCitizen<-as.factor(cleanData$SeniorCitizen)

# the 'No phone service' response to 'No' for the MultipleLines variable.
cleanData$MultipleLines <- ifelse(cleanData$MultipleLines=="No phone service" | cleanData$MultipleLines=="No","No","Yes")
cleanData$OnlineSecurity <- ifelse(cleanData$OnlineSecurity=="No internet service" | cleanData$OnlineSecurity=="No","No","Yes")
cleanData$OnlineBackup <- ifelse(cleanData$OnlineBackup=="No internet service" | cleanData$OnlineBackup=="No","No","Yes")
cleanData$DeviceProtection <- ifelse(cleanData$DeviceProtection=="No internet service" | cleanData$DeviceProtection=="No","No","Yes")
cleanData$TechSupport <- ifelse(cleanData$TechSupport=="No internet service" | cleanData$TechSupport=="No","No","Yes")
cleanData$StreamingTV <- ifelse(cleanData$StreamingTV=="No internet service" | cleanData$StreamingTV=="No","No","Yes")
cleanData$StreamingMovies <- ifelse(cleanData$StreamingMovies=="No internet service" | cleanData$StreamingMovies=="No","No","Yes")

# change chr/int datatypes into factors
cleanData$gender<-as.factor(cleanData$gender)
cleanData$SeniorCitizen<-as.factor(cleanData$SeniorCitizen)
cleanData$Partner<-as.factor(cleanData$Partner)
cleanData$Dependents<-as.factor(cleanData$Dependents)
cleanData$PhoneService<-as.factor(cleanData$PhoneService)
cleanData$MultipleLines<-as.factor(cleanData$MultipleLines)
cleanData$InternetService<-as.factor(cleanData$InternetService)
cleanData$OnlineSecurity<-as.factor(cleanData$OnlineSecurity)
cleanData$OnlineBackup<-as.factor(cleanData$OnlineBackup)
cleanData$DeviceProtection<-as.factor(cleanData$DeviceProtection)
cleanData$TechSupport<-as.factor(cleanData$TechSupport)
cleanData$StreamingTV<-as.factor(cleanData$StreamingTV)
cleanData$StreamingMovies<-as.factor(cleanData$StreamingMovies)
cleanData$Contract<-as.factor(cleanData$Contract)
cleanData$PaperlessBilling<-as.factor(cleanData$PaperlessBilling)
cleanData$PaymentMethod<-as.factor(cleanData$PaymentMethod)
cleanData$Churn<-as.factor(cleanData$Churn)

str(cleanData)


# split data 70% training 30% validation
set.seed(12345)
sample_size = floor(0.7*nrow(cleanData))

split_data <- sample(1:nrow(cleanData), size = sample_size)
training_data<- cleanData[split_data,]
testing_data<- cleanData[-split_data,]

#
prop.table(table(training_data$Churn))
prop.table(table(testing_data$Churn))

logModel <- glm(training_data$Churn~., data=training_data, family=binomial(link="logit"))
summary(logModel)

lr_prob1 <- predict(logModel, testing_data, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"Yes","No")
table(Predicted = lr_pred1, Actual = testing_data$Churn)

lr_prob2 <- predict(logModel, training_data, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"Yes","No")
lr_tab1 <- table(Predicted = lr_pred2, Actual = training_data$Churn)
lr_tab2 <- table(Predicted = lr_pred1, Actual = testing_data$Churn)
lr_acc <- sum(diag(lr_tab2))/sum(lr_tab2)
lr_acc


library(C50)
library(caret)
decisionModel <- C5.0(training_data[,1:19], 
                      training_data$Churn)
summary(decisionModel)
preds1 <- predict(decisionModel, training_data, type="class")
confusionMatrix(preds1, training_data$Churn)

a <- predict(decisionModel, training_data, type="prob")
sensitivity(training_data$Churn,a)

decisionModel1 <- C5.0(training_data[,1:19], 
                      training_data$Churn,
                      trials=10)
decisionModel1
summary(decisionModel1)
plot(decisionModel1)
qol_boost_pred6 <- predict(decisionModel1, testing_data[ ,-20])
confusionMatrix(table(predicted = qol_boost_pred6, actual = testing_data$Churn))

#decisionModel1 <- C5.0(training_data[,c(5,8,15)], 
#                     training_data$Churn)
#plot(decisionModel, type = "simple")
plot(decisionModel[3])


# Apply the decision tree model to the validation data
preds <- predict(decisionModel, testing_data, type="class")


# confusion matrix (Use validation data to test the tree model)
confusionMatrix(preds, testing_data$Churn)






# correlation coefficients and p-value between variables 
# install.packages("psych")
#library("psych")
#correlationdata <- corr.test(projectdata[2:21])
# correlation coefficients
#correlationdata$r
# p-value
#correlationdata$p
library(pROC)

roc_lm <- roc(testing_data$Churn,lr_prob1,plot=T, legacy.axes=T, percent=T, 
              xlab="1-Specificity(False Postitive %)",
              ylab="Sensitivity(True Postitive %)")

pred_dm <- predict(decisionModel,testing_data,type = 'prob')

roc_dm <- roc(testing_data$Churn,pred_dm[,2],plot=T, legacy.axes=T, percent=T,
              xlab="1-Specificity(False Postitive %)",
              ylab="Sensitivity(True Postitive %)",
              main = "ROC analysis for Logistic vs. Decision Tree Model")
# to add to the same graph: add=TRUE
plot(roc_lm, col = "red", lty = 1, add = TRUE, print.auc=T, legacy.axes=T,
     main = "ROC analysis for Logistic Regression - Decision Tree Model")
plot(roc_dm, col = "black", ltpred = 10, add = TRUE,print.auc=T, 
     print.auc.y=40,legacy.axes=T)
legend("bottomright", c('Logistic','Decision'),lty=c(1,1),lwd=c(2,2),col=c('red','black'))







