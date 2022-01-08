# install.packages("C50")
# install.packages("modeldata")
# install.packages("e1071")

# read csv data file
myData<-read.table("credit.csv",sep=",",header=T)

# check class for each column
str(myData)
colnames(myData)


######          Q1
# create dummy variables for
# (CHK_ACCT, SAV_ACCT, HISTORY, JOB, TYPE and NUM_CREDITS )
myData$CHK_ACCT<-as.factor(myData$CHK_ACCT)
myData$SAV_ACCT<-as.factor(myData$SAV_ACCT)
myData$HISTORY<-as.factor(myData$HISTORY)
myData$JOB<-as.factor(myData$JOB)
myData$TYPE<-as.factor(myData$TYPE)
myData$NUM_CREDITS<-as.factor(myData$NUM_CREDITS)

# check class for each column
sapply(myData, class)

# Create a new column (Default) based on the NPV
Default = ifelse(myData$NPV>0,"No","Yes")
myData$Default <- Default
# Change Default variable type into factor
class(myData$Default)
myData$Default <- as.factor(myData$Default)
class(myData$Default)

# using the variables from Column 2 to 20
# get rid of unnecessary column ("OBS", "AMOUNT_REQUESTED", "NPV")
myData <- myData[-c(1,21,22)]
colnames(myData)


######          Q2
# Split the dataset into training (666 records) and 
# validation (334 records) with the set.seed(12345) command
set.seed(12345) 
train = sample(1:nrow(myData),666) 
test = -train

training_data = myData[train, ]
testing_data = myData[test, ]
training_Default = Default[train]
testing_Default = Default[test]

# To build a decision tree model with C5.0 function
library(C50)
decisionModel <- C5.0(training_data[,1:19], 
                      training_data$Default)
summary(decisionModel)
# cat("Prediction accuracy (Training) = (455+99)/666*100% = 83.2%")


# 2nd way: training validation (confusion matrix)
C50_predict_train = predict(decisionModel, training_data, type = "class")
error=ifelse(C50_predict_train!=training_Default,1,0)
sum(error)
table(predicted = C50_predict_train, actual = training_data$Default)
# cat("Prediction accuracy (Training) = (455+99)/666*100% = 83.2%")

# plot(decisionModel, subtree = 32)


######          Q3
# Apply the model to the testing dataset
preds <- predict(decisionModel, testing_data, type="class")
preds

library(caret)
# confusion matrix (testing dataset)
confusionMatrix(preds, testing_data$Default)
# cat("Prediction accuracy (Testing) = (208+38)/334*100% = 73.65%")


# 2nd way: testing validation (confusion matrix)
error=ifelse(preds!=testing_Default,1,0)
sum(error)
#######generate a misclassification table
with(testing_data, table(predicted = preds, actual = Default))
# cat("Prediction accuracy (Testing) = (208+38)/334*100% = 73.65%")



######## Alternative tree() function
######## To build a decision tree model with tree function
library(tree)
tree_model = tree(Default~., training_data)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty=0)

# training validation (confusion matrix)
tree_predict_train = predict(tree_model, training_data, type = "class")
error=ifelse(tree_predict_train!=training_Default,1,0)
sum(error)
table(predicted = tree_predict_train, actual = training_data$Default)
# cat("Prediction accuracy (Training) = (455+55)/666*100% = 76.58%")

########Alternative visualization
# install.packages("maptree")
library(maptree)
draw.tree(tree_model,cex=0.8)

#######check the model's performance using the testing dataset
tree_predict_test=predict(tree_model, testing_data, type="class")
error=ifelse(tree_predict_test!=testing_Default,1,0)
sum(error)

#######generate a misclassification table
with(testing_data, table(predicted = tree_predict_test, actual = Default))
# cat("Prediction accuracy (Testing) = (209+20)/334*100% = 68.56%")

