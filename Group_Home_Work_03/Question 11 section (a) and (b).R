rm(list = ls())
dev.off()
library("ISLR")
library("MASS")
autop = read.csv("ISLR"$"Auto.csv")
autop = ISLR::Auto
attach(autop)

## 11 (a) 
## create a binary variable that contains 1 if mpg>median and 0 if mpg<median
mpg01 = factor(mpg, ordered = F, levels = c(0,1))
mpg01 = ifelse(mpg>median(mpg), 1, 0)
autop = data.frame(autop,mpg01)
summary(autop)

## 11 (b)
## explore the data graphically and determine which of the other features are most likely to predict mpg01

pairs(autop, lower.panel = NULL)
## mpg01 is a direct product of mpg. 
## Since horsepower, weight, displacement, and acceleration features affect mpg, they affect mpg01 as well.
## We boxplot mpg and each of these features to investigate how they might affect mpg01

boxplot(horsepower~mpg01, data = autop)

## above boxplot tells us that we can predict that cars with high horsepower would have mpg01=0 (mpg below median).
## the highest observation of the mpg01=1 category (neglecting outliers) is about equal to the median observation of mpg01=0 category.

boxplot(weight~mpg01, data = autop)

## above boxplot tells us that we can predict that heavy cars would belong to the mpg01=0 category (mpg below median).
## the median observation of the mpg01=1 category is about equal to the smallest observation of mpg01=0! 
## If a car weighs over 3500 we can certainly predict it will belong to the mpg01=0 category, and half of the cars in the Auto dataset weigh more than 3500. 

boxplot(displacement~mpg01, data = autop)

## above boxplot tells us that we can predict that cars with high displacement would belong to the mpg01=0 category (mpg below median).
## all of the observations of the mpg01=1 category fit in the first quartile of the mpg01=0 category.
## Using the displacement feature, we can predict over 75% of the cars would belong in the mpg01=0 category.
## This feature seems to be the most useful in trying to predict mpg01.

boxplot(acceleration~mpg01, data = autop)

## above boxplot tells us that it would be very hard to use the acceleration feature to predict mpg01, since the two categories distribute very similarly.


