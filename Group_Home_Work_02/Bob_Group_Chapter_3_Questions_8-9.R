# HOMEWORK #2
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


# IMPORT AND MODIFY/CLEAN DATA
web = "http://faculty.marshall.usc.edu/gareth-james/ISL/Auto.data"
Auto=read.table(web,header=T,na.string="?")
dim(Auto)
colSums(is.na(Auto))
Auto=na.omit(Auto)
dim(Auto)
attach(Auto)

# 8. This question involves the use of simple linear regression on the 
# Auto data set.
# 8. (a) Use the lm() function to perform a simple linear regression 
# with mpg as the response and horsepower as the predictor. Use the summary() 
# function to print the results. Comment on the output.
lm.fit = lm(Auto$mpg~Auto$horsepower)
lm.fit
summary(lm.fit)

# 8. i. Is there a relationship between the predictor and the response?
#### Comments
# Yes, there is a connection between horsepower and mpg

# 8. ii. How strong is the relationship between the predictor and the response?
#### Comments
# The correlation is strong: -0.78

# 8. iii. Is the relationship between the predictor and the response positive 
# or negative?
#### Comments
# The correlation is negative

# 8. iv. What is the predicted mpg associated with a horsepower of 98? 
# What are the associated 95 % confidence and prediction intervals?
predict(lm(mpg~horsepower),data.frame(horsepower=98),interval="confidence")
predict(lm(mpg~horsepower),data.frame(horsepower=98),interval="prediction")

# 8. (b) Plot the response and the predictor. 
# Use the abline() function to display the least squares regression line.

plot(Auto$horsepower,Auto$mpg, pch=1, col="darkgreen")
abline(lm.fit, col="red", lwd=4) 
# 8. (c) Use the plot() function to produce diagnostic plots of the 
#least squares regression fit. Comment on any problems you see with the fit.

par(mfrow=c(2,2))
plot(lm.fit)

#### Comments:
# In the residual vs. fitted model, the plots show a U shape pattern, 
# that is violating the linearity. In the second model, The normal Q-Q, 
# the plot shows there is a linear relationship. The 3rd model represents 
# the variance of the error stayed the same and the 4th model showed there were 
# few outliers and leverage but the fitted line violated linearity


# 9: MULTIPLE REGRESSION 
#9a produce scatterplot of all the data
names(Auto)
pairs( Auto, lower.panel = NULL)

#9b matrix of correlations
cor(Auto[,-9])

#9c multiple liniar regression with mpg as response variable
fit=lm(Auto$mpg~
         Auto$cylinders+
         Auto$displacement+
         Auto$horsepower+
         Auto$weight+
         Auto$acceleration+
         Auto$year+
         Auto$origin
)
summary(fit)
coefficients(fit)

# 9. i. Is there a relationship between the predictors and the response?
#### Comments
# Yes, there is a relationship between the predictor and the response, 
# we confirmed it by looking at the H null hypothesis. The p-value corresponds 
# the f stats at 2.037

# 9. ii. Which predictors appear to have a statistically significant 
# relationship to the response?

#### Comments
# All the predictors are statistically significant, excluding “cylinder”, 
# “horsepower” and “acceleration”  because  p-value associated each predictor’s 
# t-statistics 

# 9. iii. What does the coefficient for the year variable suggest?

#### Comments
# The coefficient for the year suggests that as we increase the year by 
# 1 the fuel-efficient also increases by 0.750 while the other predictor 
# value stays the same. For example, the car became more fuel-efficient every 
# year.

#9d produce diagnostic plots
par(mfrow=c(2,2))
plot(fit)

#### Comments
# The residual vs. fitted model indicated a curvier line and that violates 
# the linearity. The Q-Q plot does satisfy the linearity because of having 
# a straight fitted line with a positive strong correlation between standard 
# residual and theoretical quantiles. The scale location model shows a lot of 
# unusual outliers in the model but the standard residual vs leverage have shown 
# fewer outliers and have a high leverage point in the model.


#9e use * and : to add interactions effects
fitNew=lm(Auto$mpg~Auto$year*Auto$acceleration)
summary(fitNew)
par(mfrow=c(2,2))
plot(fitNew)

#### Comments
# When we look at the p-value the association between the displacement and 
# the weight seems to be more statistically significant, however, the 
# association between the cylinder and displacement does not look statistically 
# significant. 

## 9. (f) Try a few different transformations of the variables, such as log(X), 
# √X, X2. Comment on your findings.
plot( log(horsepower), mpg, abline(lm( mpg~log(horsepower))))
plot( sqrt(horsepower), mpg, abline(lm(mpg~sqrt(horsepower))))
plot( (horsepower)^2, mpg, abline(lm(mpg~I(horsepower^2))))


#### Comments
# When we used “horsepower” as our single predictor and to try different 
# transformation, we noticed only the log transformation gave us the most 
# liner looking plot for the “horsepower” 	