# HOMEWORK #4
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

## Chapter: 08
## Questoin: 12 
## Apply boosting, bagging, and random forests 
## to a data set of your choice. Be sure to fit 
## the models on a training set and to evaluate 
## their performance on a test set. How accurate 
## are the results compared to simple methods like 
## linear or logistic regression? Which of these 
## approaches yields the best performance?

# install.packages("ggplot2")
install.packages("googledrive")


## Chapter: 09
## Question: 04
## Generate data: ( a two class system )
# 1) 100 observations 
# 2) 2 visible non-linear variables for the two classes 

## TASK: Show the following and provide graphs and plots as evodence
# A support vector with a polynomial kernal or
# a rafial kernial will out perform a support vector classifier