#k-means clustering

#set the working directory properly

mydata<-read.table("Wholesale_customers_data.csv", sep=",", header=T)

head(mydata)

km_3<- kmeans(mydata[,3:8], 3)
km_3

library(factoextra)  #extra package

fviz_cluster(km_3, mydata[,3:4], ellipse.type = "norm")


km_5<- kmeans(mydata[,3:8], 5)
km_5

fviz_cluster(km_5, mydata[,7:8], ellipse.type = "norm")

#elbow method

set.seed(123)

fviz_nbclust(mydata, kmeans, method = "wss")

#####standardize

scale_data<-data.frame(mydata[,1:2],scale(mydata[,3:8]))

head(scale_data)

km_3_new<- kmeans(scale_data[,3:8], 3)
km_3_new

fviz_cluster(km_3_new, scale_data[,7:8], ellipse.type = "norm")


km_5_new<- kmeans(scale_data[,3:8], 5)
km_5_new

fviz_cluster(km_5_new, scale_data[,3:8], ellipse.type = "norm")


set.seed(123)

fviz_nbclust(scale_data, kmeans, method = "wss")