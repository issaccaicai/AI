# load data
mydata<-read.csv("College.csv", header=TRUE)

# add new column: acceptance rate
mydata$accept_rate<-mydata$Accept/mydata$Apps

# new data set: university/college with acceptance rate
top3lowest<-mydata[,c(1,20)]


# Q1: Top 3 university/college that has the lowest acceptance rate
head(top3lowest[order(top3lowest$accept_rate),], n =3)


# Q2:
# Distribution of full-time undergraduate for private universities/colleges
hist(mydata$F.Undergrad[mydata$Private == "Yes"], 
main = "Distribution of F.Undergrad for private colleges", 
xlab = "Number of full-time undergraduate", las=1, col="light blue")

# Distribution of full-time undergraduate for public universities/colleges
hist(mydata$F.Undergrad[mydata$Private == "No"], 
     main = "Distribution of F.Undergrad for public colleges", 
     xlab = "Number of full-time undergraduate", las=1, col="light blue")

# Distribution of part-time undergraduate for private universities/colleges
hist(mydata$P.Undergrad[mydata$Private == "Yes"], 
     main = "Distribution of P.Undergrad for private colleges", 
     xlab = "Number of Part-time undergraduate", las=1, col="light blue")

# Distribution of part-time undergraduate for public universities/colleges
hist(mydata$P.Undergrad[mydata$Private == "No"], 
     main = "Distribution of P.Undergrad for public colleges", 
     xlab = "Number of Part-time undergraduate", las=1, col="light blue")


# Q3
# Create a new column called "Elite"
mydata$Elite <- "No"
mydata$Elite[mydata$Top10perc > 50] <- "Yes"

# change Elite column as factor
mydata$Elite = as.factor(mydata$Elite)
# Use the summary() function to check how many Elite schools there are
summary(mydata$Elite)

# produce side-by-side boxplots of "Outstate" versus "Elite"
boxplot(mydata$Outstate~mydata$Elite, main = "Outstate versus Elite", 
        xlab = "Elite", ylab = "Outstate")

