#Q11, a)
library(ISLR)
summary(Auto)

attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

#Q11, b)

cor(Auto[, -9]) # Fix it -Didn't work
pairs(Auto)
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")


boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")


boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")


boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")


boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")


boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

#Q11, c)
train = (year%%2 == 0)  # if the year is even
test = !train
Auto.train = Auto[train, ]
Auto.test = Auto[test, ]
mpg01.test = mpg01[test]

#Q11, d)
# LDA
library(MASS)
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
lda.pred = predict(lda.fit, Auto.test)
mean(lda.pred$class != mpg01.test)

#Q11, e)
# QDA
qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
qda.pred = predict(qda.fit, Auto.test)
mean(qda.pred$class != mpg01.test)

#Q11, f)
# Logistic regression
glm.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              family = binomial, subset = train)
glm.probs = predict(glm.fit, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)


#Q11, g)
#KNN - don't work
#https://rpubs.com/ppaquay/65560
library(class)
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 = mpg01[train]


set.seed(1)
# KNN(k=1)
pred.knn = knn(train.X, test.X, train.mpg01, k = 1)
table(pred.knn, mpg01.test)

mean(pred.knn != mpg01.test)

# KNN(k=10)
pred.knn = knn(train.X, test.X, train.mpg01, k = 10)
table(pred.knn, mpg01.test)

mean(pred.knn != mpg01.test)

# KNN(k=100)
pred.knn <- knn(train.X, test.X, train.mpg01, k = 100)
table(pred.knn, mpg01.test)

mean(pred.knn != mpg01.test)






