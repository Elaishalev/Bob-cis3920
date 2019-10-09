# HOMEWORK #2
# GROUP: Bob 
# Maryla Wozniak
# Shakila Hoque
# Elai Shalev 
# Matthew Perez

# GENERAL SETUP
rm(list = ls())
dev.off()

# IMPORT CLEAN DATA

# install.packages("MASS"")
# install.packages("ISLR")
library("MASS")
library("ISLR")
Auto_local = Auto
attach(Auto_local)

# Question 11: Develop a model to predict whether a car has high/low mpg.
## (A) DEFINE BINARY VARABLE 'mpg01' MPG01 = 1 IF MPG > MEDIAN(MPG) ELSE 0

Auto_local[["mpg01"]] =  as.integer(Auto_local$mpg > median(Auto_local$mpg))


## (B) VISUAL DATA EXPLORATION
### WHICH FEATURES ARE LIKELY TO HAVE THE GREATEST IMPACT? 
### DESCRIBE YOUR FINDINGS

# PAIRWISE MATRIX PLOT EXCLUDING MPG AND NAME
pairs(Auto_local[, c(-1, -2,-8,-9, -10)], lower.panel = NULL)

## (C) SPLIT DATA INTO TEST AND TRAINING SETS


## (D) PERFORM LDA ON THE TRAINGING SET TO PREDICT 'mpg01' 
### WHAT IS THE TEST ERROR?


## (E) PERFORM QDA ON THE TRAINGING SET TO PREDICT 'mpg01' 
### WHAT IS THE TEST ERROR?


## (F) PERFORM LOGISTIC REGRESSION ON THE TRAINING DATA IN ORDER TO PREDICT 'mpg01'
### WHAT IS THE TEST ERROR?



# OPTIONAL
## (G) PERFORM KNN ON THE TRAINING DATA WITH SEVERAL VALUES FOR 'K' IN ORDER TO PREDICS 'mpg01' 
### WHAT IS THE TEST ERROR?
### WHAT VALUE OF K PERFORMS BEST?
