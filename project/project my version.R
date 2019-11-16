getwd()
rm(list = ls())
dev.off()

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(rbokeh))
suppressMessages(library(leaflet))

data=read.csv("listings.csv",header = T,na.string="")
data=subset (data, select=-c(number_of_reviews,last_review,reviews_per_month ))
names(data)
class(data)
summary(data)
dim(data)

attach(data)

#cleaning data
colSums(is.na(data))
data=subset(data, minimum_nights<365 )
data=data[!(data$price==0), ]

data [is.na(data$last_review)] = 0
data[data$number_of_reviews == "0", "reviews_per_month"] = 0


plot ( neighbourhood_group, room_type)
plot ( neighbourhood_group, price)
plot ( room_type, price)
plot (neighbourhood_group, minimum_nights)

par(mfow=c(1,1,1,1))
coco=lm(price~room_type,neighbourhood_group, data)
summary(coco)
plot(room_type,price,neighbourhood_group)
