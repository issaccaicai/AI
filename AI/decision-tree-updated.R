##############3decision tree example updated
## Make sure the R version is up-to-date


#install and load the package
#load the dataset
#exploration
#create the outcome variable (i.e., the label)
#split the dataset into training and test
#run the tree algorithm to build the model
#visualize the model
#make prediction

install.packages("ISLR")  #the dataset is from this package
install.packages("tree")  #the decision tree model is from this package

library(ISLR)
data(package="ISLR")
carseats<-Carseats

library(tree)

######exploration of the dataset

head(carseats)
colnames(carseats)
hist(carseats$Sales)
hist(carseats$Price)

######Create the binary outcome variable based on Sales
High=ifelse(carseats$Sales>8,"Yes","No")
carseats = data.frame(carseats,High)
###check the type of the variable "High", make sure it is of factor type
class(carseats$High)
carseats$High=as.factor(carseats$High)

carseats=carseats[,-1]
names(carseats)

#########Split the data into training and test

#set the seed of R's random number generator
#so that all the results, figures, etc. are reproducible.
set.seed(2) 
train=sample(1:nrow(carseats),200) #adjust the size of the training set
test=-train

training_data=carseats[train,]
testing_data=carseats[test,]
testing_high=High[test]

##########Build the model
tree_model = tree(High~., training_data)
plot(tree_model)
text(tree_model, pretty=0)


########Alternative visualization
#install.packages("maptree")
library(maptree)
draw.tree(tree_model,cex=0.8)

#######check the model's performance using the test data
######note that it is a classification tree, so specify the type as "class"

tree_predict=predict(tree_model, testing_data, type="class")
error=ifelse(tree_predict!=testing_high,1,0)
sum(error)

#######generate a misclassification table
with(carseats[-train,], table(tree_predict, High))
