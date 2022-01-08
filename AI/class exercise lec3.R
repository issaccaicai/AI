x<-c(0,1,2,3,4)
y<-1:5
z<-1:50

x+y
x*y
x*z

x<-c(0,1,2,3,4)
x[2]
x[1:3]
x[-1]
x[c(1,2,3)]

# matrix
x = c(1,3,2,5,7,3,5,6,2,5,8,9,9,10,20)
y = matrix(x,5,3)
y

x = c(1,3,2,5,7,3,5,6,2,5,8,9,9,10,20)
y = matrix(x,5,3,byrow=T)
y

y[1,1]
y[2,3]
y[,1]
y[1,]
y[3:5,2:3]

y[c(1,4),c(1,3)]

# dataframe
w <- c("a","b","c","d")
w
v <- c(1:4)
v
df = data.frame(w,v)
df

df1 = data.frame(v,w)
df1

# mydata<-read.csv("House.csv",header=TRUE)
# mydata<-read.table("House.csv",sep=",",header=TRUE)

# nrow(df)
# ncol(df)
# class(df$v)
# sapply(df,class)
