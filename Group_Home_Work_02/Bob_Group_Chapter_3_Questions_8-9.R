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