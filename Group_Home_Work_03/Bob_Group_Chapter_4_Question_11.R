# HOMEWORK #3
# GROUP: Bob 
# Maryla Wozniak
# Shakila Hoque
# Elai Shalev 
# Matthew Perez

# GENERAL SETUP

# Clear all existing variables in global environment
rm(list = ls())
# CLear plot tab and close/save any open files 
dev.off()
# Set the working dorectory to the location where the file was saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path))



# IMPORT CLEAN DATA

# Install the ISLR library so we can access the cleaned data set 
# for the following quesiotn

install.packages("ISLR")

# Save local copy of ISLR's Auto data  
Auto_local = ISLR::Auto

# Attach reference varables from data set 
attach(Auto_local)




# Question 11: Develop a model to predict whether a car has high/low mpg.
## (A) DEFINE BINARY VARABLE 'mpg01' MPG01 = 1 IF MPG > MEDIAN(MPG) ELSE 0

Auto_local[["mpg01"]] = factor(
  as.integer(Auto_local$mpg > median(Auto_local$mpg)), 
  ordered = F, 
  levels = c(0,1))
summary(Auto_local)


## (B) VISUAL DATA EXPLORATION
### WHICH FEATURES ARE LIKELY TO HAVE THE GREATEST IMPACT? 
### DESCRIBE YOUR FINDINGS

## INITIAL PLOT
pairs(Auto_local, lower.panel = NULL)



## mpg01 is a direct product of mpg. 
## Since horsepower, weight, displacement, 
## and acceleration features affect mpg, they 
## affect mpg01 as well. We boxplot mpg and each 
## of these features to investigate how they 
## might affect mpg01

boxplot(horsepower~mpg01, data = Auto_local, main = "Horsepower and mpg Boxplot")



## above boxplot tells us that we can predict 
## that cars with high horsepower would have 
## mpg01=0 (mpg below median). The highest observation 
## of the mpg01=1 category (neglecting outliers) is 
## about equal to the median observation of mpg01=0 category.

boxplot(weight~mpg01, data = Auto_local, main = "Weight and mpg Boxplot")



## above boxplot tells us that we can predict that 
## heavy cars would belong to the mpg01=0 category 
## (mpg below median). The median observation of the 
## mpg01=1 category is about equal to the smallest 
## observation of mpg01=0! If a car weighs over 3500 
## we can certainly predict it will belong to the 
## mpg01=0 category, and half of the cars in the Auto 
## dataset weigh more than 3500. 

boxplot(displacement~mpg01, data = Auto_local, main = "Displacement and mpg Boxplot")



## above boxplot tells us that we can predict that 
## cars with high displacement would belong to the 
## mpg01=0 category (mpg below median). All of the 
## observations of the mpg01=1 category fit in the 
## first quartile of the mpg01=0 category. Using the 
## displacement feature, we can predict over 75% of 
## the cars would belong in the mpg01=0 category. This 
## feature seems to be the most useful in trying to 
## predict mpg01.

boxplot(acceleration~mpg01, data = Auto_local, main = "Acclereration and mpg Boxplot")

## Above boxplot tells us that it would be very hard to use the acceleration feature to predict mpg01, since the two categories distribute very similarly.



## (C) SPLIT DATA INTO TEST AND TRAINING SETS

set.seed(1)
num_train <- nrow(Auto_local) * 0.75

inTrain <- sample(nrow(Auto_local), size = num_train)

training <- Auto_local[inTrain,]
testing <- Auto_local[-inTrain,]

training
testing

## First, we are omitting the cars with 3 or 5 cylinders 
## becasue there are only few of them, it will likely 
## skew the analysis. 


## (D) PERFORM LDA ON THE TRAINGING SET TO PREDICT 'mpg01' 
### WHAT IS THE TEST ERROR?

require(MASS)

## The following formula will be used for each of the 
## following models
fmla <- as.formula('mpg01 ~ displacement + horsepower + weight + year + cylinders')

lda_model <- lda(fmla, data = training)

lda_pred <- predict(lda_model, testing)

table(lda_pred$class, testing$mpg01)

1 - mean(lda_pred$class == testing$mpg01)

#The LDA model works great!
#The test error is 0.122449 


## (E) PERFORM QDA ON THE TRAINGING SET TO PREDICT 'mpg01' 
### WHAT IS THE TEST ERROR?
qda_model = qda(fmla, data = training)

qda_pred <- predict(qda_model, testing)

table(qda_pred$class, testing$mpg01)

1 - mean(qda_pred$class == testing$mpg01)

## QDA modal test error is still the same as LDA, however 
## the test results for each value is different. Example; 
## the LDA model has a higher rate of error for values 
## over median but the QDA have more equal distribution 
## in the error for test value. 



## (F) PERFORM LOGISTIC REGRESSION ON THE TRAINING DATA IN ORDER TO PREDICT 'mpg01'
### WHAT IS THE TEST ERROR?

log_reg <- glm(fmla, data = training, family = binomial)

pred <- predict(log_reg, testing, type = 'response')
pred_values <- round(pred)
table(pred_values, testing$mpg01)

1- mean(pred_values == testing$mpg01)

## The Legistic Regressional model seems similar to LDA and 
## QDA model, eventhough it did not perform as well as 
## other two model. The error rate is 0.09183673


# OPTIONAL
## (G) PERFORM KNN ON THE TRAINING DATA WITH SEVERAL VALUES FOR 'K' IN ORDER TO PREDICS 'mpg01' 
### WHAT IS THE TEST ERROR?
### WHAT VALUE OF K PERFORMS BEST?


