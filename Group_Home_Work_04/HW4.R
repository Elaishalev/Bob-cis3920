# cleaning the environment and importing dataset

rm(list = ls())
dev.off()
suppressMessages(library(randomForest))
library(gbm)
library(tree)
nba = read.csv("CIS_HW4.csv")
names(nba)
nba = nba[,-c(1,2,3,7,11,14,17,18,21)]
attach(nba)
# omit all observations with less then 6 
# minutes of play time since they would
# make the data more bias

nba = nba[nba$MP>6, ]

# check for NA values in the data 

colSums(is.na(nba))

#splitting the data into training and testing

set.seed(8)
train_nba = sample(1:nrow(nba), nrow(nba)/2)

# applying boosting

boost_nba = gbm(PTS~.-Tm, data = nba[train_nba,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost_nba)
par(mfrow = c(2,2))
plot(boost_nba, i="FG")
plot(boost_nba, i="FGA")

yhat.boost = predict(boost_nba, newdata = nba[-train_nba,], n.trees = 5000)
mean((yhat.boost - nba[-train_nba ,"PTS" ])^2)

# MSE for boosting is 6475331


# applying bagging

bag_nba = randomForest(PTS~.-Tm ,data = nba, subset = train_nba, mtry = 19, importance = T)
yhat.bag = predict(bag_nba, newdata = nba[-train_nba,])
plot(yhat.bag, nba[-train_nba,"PTS"])
abline(0,1, col = 5)
mean((yhat.bag - nba[-train_nba ,"PTS" ])^2)

#MSE for bagging is 0.3524371


# applying random forest, cross-validating, and pruning the tree

tree_nba = tree(PTS~.-Tm, nba , subset = train_nba)
summary(tree_nba)
plot(tree_nba)
text(tree_nba, pretty = 0)

cv_nba = cv.tree(tree_nba)
plot(cv_nba$size, cv_nba$dev, type = 'b')
cv_nba

pruning_nba = prune.tree(tree_nba, best = 8)
plot(pruning_nba)
text(pruning_nba, pretty = 0)

yhat.tree = predict(tree_nba, newdata = nba[-train_nba,])
plot(yhat.tree, nba[-train_nba,"PTS"])               
abline(0,1, col = 5)
mean((yhat.tree-nba[-train_nba,"PTS"])^2)

# MSE for the decision tree is 1.807063


# applying multiple linear regression
par(mfrow = c(2,2))
lm_nba = lm(PTS~.-Tm, nba[train_nba,])
plot(lm_nba)
summary(lm_nba)
lm_nba.predict = predict(lm_nba, newdata = nba[-train_nba,])
lm_nba.predict[1:5]
mean((lm_nba.predict-nba[-train_nba,"PTS"])^2)
par(mfrow = c(1,1))

# MSE for the Linear Regression is 0.005425117

# We noted that 3 obs. are outliers 
# in the training data (#11, 14, 42).
# in the residuals vs. leverage plot we see that
# 14 and 42 influence the model, so we will omit
# them and see if it fits better.

train_nba_lm = train_nba[-c(14,42)]
lm_nba_2 = lm(PTS~.-Tm, nba[train_nba_lm,])
plot(lm_nba_2)
lm_nba.predict_2 = predict(lm_nba_2, newdata = nba[-train_nba_lm,])
mean((lm_nba.predict_2-nba[-train_nba_lm,"PTS"])^2)

# MSE for the Linear Regression is 0.00551884.
# The MSE remained almost the same.


# applying logistic regression

log_nba = glm(PTS~.-Tm, data = nba[train_nba,])
yhat.log = predict(log_nba, newdata = nba[-train_nba,])
plot(yhat.log, nba[-train_nba,"PTS"])
abline(0,1, col = 5)
mean((yhat.log - nba[-train_nba ,"PTS" ])^2)

# MSE for the Linear Regression is 0.005228678


# Seems like the logistic regression is the best model
# to predict the amount of points a certain player will 
# score based on other statistics in our data.