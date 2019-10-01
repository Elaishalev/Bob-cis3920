# HOMEWORK #2
# GROUP: Bob 
# Maryla Wozniak
# Shakila Hoque
# Elai Shalev 
# Matthew Perez

# GENERAL SETUP
rm(list = ls())
dev.off()

# IMPORT LIBRARIES FOR REGRESSION


# IMPORT AND MODIFY/CLEAN DATA
file_path = "../Data_Sources/Auto.data"
data = read.table(file_path)

# SET CATAGORICAL DATA

#8a
#previuos data cleanup
rm(list = ls())
dev.off()
#read in table
Auto=read.table("Auto.data")
Auto=read.table("Auto.data",header=T,na.string="?")

#cheking for missing data
colSums(is.na(Auto))
Auto=na.omit(Auto)
fix(Auto)
dim(Auto)
is.na(Auto)
attach(Auto)
lm.fit=lm(mpg~horsepower)
lm.fit
summary(lm.fit)

cor(horsepower,mpg)
#8 i:  yes, there is a connection between horsepower and mpg
#8 ii: correlation is strong : -.78
#8 iii: correlation is negative
predict(lm.fit,data.frame(horsepower=98),interval="confidence")
predict(lm.fit,data.frame(horsepower=98),interval="prediction")

#8iv: predicted mpg associated with horsepower of 98 is 24.47
# associated 95% confidence interwals is (23.97,24.96)
#associated 95% prediction interwals is (14.8, 34.12)
#8b
abline(lm.fit,col="red",lwd=4)
#8c
plot(horsepower,mpg, pch=1, col="darkgreen")
#comments: ??????



# 9: MULTIPLE REGRESSION 
#9a produce scatterplot of all the data
names(Auto)
as.factor(cylinders)
as.factor(year)
as.factor(origin)
pairs(Auto)
#9b matrix of correlations
Auto=Auto[,-9]
cor(Auto)
#9c multiple liniar regression with mpg as response variable
fit=lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year,origin)
summary(fit)
coefficients(fit)
#notes here->

#9d produce diagnostic plots
par(mfrow=c(2,2))
plot(fit)
#9e use * and : to add interactions effects
fitNew=lm(mpg~year*acceleration)
summary(fitNew)

#9f transformatin of varibles
fit=lm(mpg~cylinders+displacement+horsepower+I(weight^2)+I(acceleration^2))
summary(fitNew)
# summary->       
