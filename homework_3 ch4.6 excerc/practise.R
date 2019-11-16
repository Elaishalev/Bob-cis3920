rm(list = ls())
dev.off()
#I already downloded mass and islr

library("MASS")
library("ISLR")
Market=Smarket
attach(Market)
names(Market)
fix(Market)
summary(Market)
cor(Market)
#look for pairs
pairs(ppp,lower.panel=NULL)
pairs(Market,lower.panel=NULL)

#create pairs on part of data
ppp=Market
ppp=Market[seq(1,1250,20),]

cor(Market[,-9])
plot(Year, Volume)
plot(Volume)
bump=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Market,family=binomial)
par(mfrow=c(2,2))
summary(bump)
plot(bump)
coef(bump)

summary(bump)$coef [,4]
view(Market)
