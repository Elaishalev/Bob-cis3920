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
file_path = "../../Data_Sources/Auto.csv"
data = read.csv(file_path)
attach(data)

# SET CATAGORICAL DATA

# QUANTITATIVE
data$horsepower = as.numeric(horsepower)

# QUALATATIVE
data$cylinders = as.factor(cylinders)
data$year = as.factor(year)
data$origin = as.factor(origin)


# 8: USING THE AUTO.CSV DATA SET
# SIMPLE LINEAR REGRESSION ON AUTO DATASET

# 8.A: USE "lm()" TO PERFORM LINEAR REGRESSION WITH "mpg" AS A RESPONCE TO "horsepower"
# PRINT RESULTS WITH "summary()"
qwerty = lm(mpg ~ horsepower)

summary(qwerty)

# 8.A.i: IS THERE A RELATIONSHIP BETWEEN THE PREDICTOR AND THE RESPONCE?


# 8.A.ii: HOW STRONG IS THE RELATIONSHIP?

# 8.A.iii: IS THE RELATIONSHIP POSETIVE OR NEGATIVE?

# 8.A.vi: WHAT IS THE PREDICTED 'mgp' ASSOCIATED WITH A 'horsepower' OF 98?
# WHAT ARE THE ASSOCIATED 95% CONFIDENCE AND PREDICTION INTERVALS?

# 8.B: PLOT THE RESPONCE AND THE PREDICTOR; 
#USE 'abline' TO PLOT THE LEAST SQUARES REGRESSION LINE

# 8.C: USE 'plot' TO PRODUCE DIAGNOSTIC PLOTS OF LEAST SQUARES REGRESSION FIT
# COMMENT ON PROBLEMS WITH THE FIT


