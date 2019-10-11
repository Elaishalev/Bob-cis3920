rm(list = ls())
dev.off
library("ISLR")
library("MASS")
qqq = Auto
attach(qqq)
mpg01 = factor(qqq$mpg, ordered = F, levels = c(0,1))
mpg01 = ifelse(mpg>median(mpg), 1, 0)
class(mpg01)
qqq = data.frame(qqq,mpg01)
summary(qqq)
fix(qqq)
median(mpg)
fix(mpg)
fix(qqq)



