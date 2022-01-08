# read csv data file
myData<-read.table("Mall_Customers.csv",sep=",",header=T)
str(myData)
# change Gender type into numeric
myData$Gender = ifelse(myData$Gender=="Male",1,0)
colSums(is.na(myData))
head(myData)
colnames(myData)

######       Q1

# Distribution of customers' annual income
hist(myData$Annual.Income..k.., 
     main = "Distribution of customers' annual income", 
     xlab = "Annual Income (k$)", 
     ylab = "Frequency",
     labels = T,
     las = 1, 
     col="light blue")

# Distribution of customers' spending score
hist(myData$Spending.Score..1.100., 
     main = "Distribution of customers' spending score", 
     xlab = "Spending Score (1-100)", 
     ylab = "Frequency",
     labels = T,
     las = 1, 
     col="light blue")


######       Q2
library(factoextra)


### annual income and spending score
df1 <- myData[,4:5]

set.seed(123)
# Run K-means clustering by setting K=3.
km_inc_spe_3 <- kmeans(df1, 3)
km_inc_spe_3

fviz_cluster(km_inc_spe_3, df1, ellipse.type = "norm")


# Run K-means clustering by setting K=5.
km_inc_spe_5 <- kmeans(df1, 5)
km_inc_spe_5

fviz_cluster(km_inc_spe_5, df1, ellipse.type = "norm")


### age and spending score
df2 <- myData[,c(3,5)]

set.seed(123)
# Run K-means clustering by setting K=3.
km_age_spe_3 <- kmeans(df2, 3)
km_age_spe_3

fviz_cluster(km_age_spe_3, df2, ellipse.type = "norm")

# Run K-means clustering by setting K=5.
km_age_spe_5 <- kmeans(df2, 5)
km_age_spe_5

fviz_cluster(km_age_spe_5, df2, ellipse.type = "norm")


### age and annual income
df3 <- myData[,c(3,4)]

set.seed(123)
# Run K-means clustering by setting K=3.
km_age_inc_3 <- kmeans(df3, 3)
km_age_inc_3

fviz_cluster(km_age_inc_3, df3, ellipse.type = "norm")

# Run K-means clustering by setting K=5.
km_age_inc_5 <- kmeans(df3, 5)
km_age_inc_5

fviz_cluster(km_age_inc_5, df3, ellipse.type = "norm")



######       Q3
set.seed(123)
fviz_nbclust(myData[,2:5], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# fviz_nbclust(myData[,2:5], kmeans, method = "silhouette")


