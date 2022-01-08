# read csv data file
projectData<-read.csv("AB_NYC_2019.csv",header=T)
str(projectData)
summary(projectData)

#checking NA values
summary(is.na(projectData))


# Price distribution
boxplot(projectData$price, horizontal = T, xaxt="n",
        main = "Price Distribution", xlab="Price($)")
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)))

# Neighbourhood Group Price Distribution
library(ggplot2)
library(scales)
## set universal plot size:
# options(repr.plot.width=10, repr.plot.height=6)
neighbourhoodPrice <- ggplot(projectData, aes(x = neighbourhood_group, y = price)) + 
  geom_boxplot() + theme_bw(base_size = 15) + scale_y_continuous(labels = dollar) + 
    labs(x="",  y = "Price($)") + ggtitle("Neighbourhood Group Price Distribution")
neighbourhoodPrice


# Room Type Price Distribution
roomTypePrice <- ggplot(projectData, aes(x = room_type, y = price))  + 
  geom_boxplot() + theme_bw(base_size = 15) + scale_y_continuous(labels = dollar) + 
  labs(x="",  y = "Price($)") + ggtitle("Room Type Price Distribution")
roomTypePrice


# Convert all columns to factor
projectData1 <- as.data.frame(unclass(projectData), stringsAsFactors=TRUE)
str(projectData1)
sapply(projectData1, class)

summary(projectData1)

# install.packages("dplyr")
library(dplyr)
library(tidyr)
airbnb_filtered_data <- projectData1 %>% 
  filter(price < quantile(projectData1$price, 0.9) & price > quantile(projectData1$price, 0.1)) %>% 
  drop_na()

summary(airbnb_filtered_data)

airbnb_filtered_data<-airbnb_filtered_data[, c(5, 7, 8, 9, 10, 11, 12, 14, 15, 16)]






# AAAAAAAA
#airbnb_filtered_data$newneighbourhood[airbnb_filtered_data$neighbourhood_group == "Bronx"] <- 1
#airbnb_filtered_data$newneighbourhood[airbnb_filtered_data$neighbourhood_group == "Brooklyn"] <- 2
#airbnb_filtered_data$newneighbourhood[airbnb_filtered_data$neighbourhood_group == "Manhattan"] <- 3
#airbnb_filtered_data$newneighbourhood[airbnb_filtered_data$neighbourhood_group == "Queens"] <- 4
#airbnb_filtered_data$newneighbourhood[airbnb_filtered_data$neighbourhood_group == "Staten Island"] <- 5

#airbnb_filtered_data$newroomtype[airbnb_filtered_data$room_type == "Entire home/apt"] <- 1
#airbnb_filtered_data$newroomtype[airbnb_filtered_data$room_type == "Private room"] <- 2
#airbnb_filtered_data$newroomtype[airbnb_filtered_data$room_type == "Shared room"] <- 3


#airbnb_filtered_data<-transform(airbnb_filtered_data, newneighbourhood = as.numeric(newneighbourhood), 
          # newroomtype = as.numeric(newroomtype))
#summary(airbnb_filtered_data$newneighbourhood)

# newairbnb_filtered_data<-airbnb_filtered_data[, c(2, 3, 5, 6, 7, 8, 9, 10, 11, 12)]







#newdata <-projectData1
#select(all_of(projectData1[, c("neighbourhood_group", "room_type")])) %>% lapply((as.numeric)
airbnb_filtered_data$neighbourhood_group_value<-as.numeric(airbnb_filtered_data$neighbourhood_group)
airbnb_filtered_data$room_type_value<-as.numeric(airbnb_filtered_data$room_type)
summary(airbnb_filtered_data)

newData <- airbnb_filtered_data[, c(2, 3, 5, 6, 7, 8, 9, 10, 11, 12)]

library("psych")
correlationdata <- corr.test(newData)
# correlation coefficients
correlationdata$r
# p-value
correlationdata$p





# set.seed(123456)

# airbnb_filtered_data <- airbnb_filtered_data %>% mutate(id = row_number())

# airbnb_train <- airbnb_filtered_data %>% sample_frac(.7) %>% filter(price > 0)

# airbnb_test  <- anti_join(airbnb_filtered_data, airbnb_train, by = 'id') %>% filter(price > 0)


# sanity check
# nrow(airbnb_train) + nrow(airbnb_test) == nrow(airbnb_filtered_data %>% filter(price > 0))




airbnb_model_1 <- lm (price ~ neighbourhood_group + latitude + longitude + room_type + minimum_nights  + number_of_reviews + reviews_per_month + calculated_host_listings_count +
                        availability_365, data = airbnb_filtered_data)
summary(airbnb_model_1)

# newProjectData<-projectData1[, c(7, 8, 10, 11, 12, 14, 15, 16)]

# get structure of the data
str(projectData1)

# checking na values in each column
colSums(is.na(projectData1))

projectData1[is.na(projectData1)] <- 0

colSums(is.na(projectData1))

# correlation coefficients and p-value between variables 
# install.packages("psych")
# library("psych")
correlationdata <- corr.test(projectData1)
# correlation coefficients
correlationdata$r
# p-value
correlationdata$p

projectModel<-lm(formula = price ~., data = projectData1)
summary(projectModel)

par(mfrow=c(2,2))
plot(airbnb_model_1)
