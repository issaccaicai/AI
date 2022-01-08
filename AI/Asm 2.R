# read csv data file
mydata<-read.table("Advertising.csv",sep=",",header=TRUE)

# get structure of the data
str(mydata)

# correlation coefficients and p-value between variables 
# install.packages("psych")
library("psych")
mycorrelation <- corr.test(mydata[2:5])
# correlation coefficients
mycorrelation$r
# p-value
mycorrelation$p

# Q1-Q3 unit increase of advertising expenditure on TV, Radio, Newspaper affect the sales
myModel<-lm(formula = sales ~ TV + radio + newspaper, data = mydata)
summary(myModel)

# Q4 diagnostic plots to examine the linearity and normality assumptions
par(mfrow=c(2,2))
plot(myModel)

