# GROUP HOMEWORK #1 
# 8: USING THE COLLEGE.CSV DATA SET
# 8: SETUP
rm(list = ls())
dev.off()

# 8.A: READ THE CSV
# CHANGE OR OMMIT VARIABLE BASED ON SOURCE FILE LOCATION RELATIVE TO R SCRIPT

file_path_to_file = "../Data_Sources/college.csv" 
CSV_file = read.csv(file_path_to_file)

# 8.B: MODIFY ROW NAMES USING FIX FUNCTION

rownames (CSV_file )=CSV_file [,1]
fix(CSV_file)

CSV_file=CSV_file[,-1]
fix(CSV_file)

attach(CSV_file)

# 8.C: MULTISTEP ANALYSIS AND DATA MANIPULATION
# 8.C.1: PRODUCE SUMMARY OF DATA SET

summary(CSV_file)

# 8.C.2: PRODUCE A SCATER PLOT OF THE FIRST 10 COLUMNS

png("8_C_2.png")
scater_plot_matrix = pairs(CSV_file[, 1:10])
dev.off()

# 8.C.3: PLOT "OUTSTATE" VS "PRIVATE" SIDE BY SIDE BOX PLOTS

png("8_C_3.png")
boxplot( CSV_file$Outstate ~ CSV_file$Private, xlab = "PUBLIC/PRIVATE INSTITUTION", ylab = "OUT OF STATE TUITION ($)", main = "Out of State Tuition:\nPublic vs Privates \nInstitutions")
dev.off()

# 8.C.4: DEFINE CATAGORICAL ATTRABUTE 'ELITE' AS TOP 10% 

Elite =rep ("No",nrow(CSV_file ))
Elite [CSV_file$Top10perc >50]=" Yes"
Elite =as.factor (Elite)
CSV_file =data.frame(CSV_file ,Elite)

summary(CSV_file)

png("8_C_4.png")
boxplot( 
        CSV_file$Outstate ~ CSV_file$Elite, 
        xlab = "ELITE/NON-ELITE INSTITUTIONS", 
        ylab = "OUT OF STATE TUITION ($)", 
        main = "Out of State Tuition:\nElite vs NON-Elite \nInstitutions"
        )
dev.off()

# 8.C.5: PLOT HISTOGRAMS

png("8_C_5(1).png")
hist(Enroll, main = "Histogram of:\nEnrolement")
dev.off()

png("8_C_5(2).png")
hist(Accept, main = "Histogram of:\nAcceptance")
dev.off()

png("8_C_5(3).png")
hist(F.Undergrad, main = "Histogram of:\nFulltime Undergrads")
dev.off()

png("8_C_5(4).png")
hist(P.Undergrad, main = "Histogram of:\nFulltime Undergrads")
dev.off()

png("8_C_5(5).png")
hist(Expend, main = "Histogram of:\nExpenditure per Student")
dev.off()

# 8.C.6: SUMMARY OF FINDINGS




# 9: USING THE "AUTO" DATA SET
# 9: SETUP
rm(list = ls())
dev.off()

file_path_to_file = "../../Data_Sources/Auto.data" 
data_file=read.table(file_path_to_file,header=T,na.strings="?")

# CHECK IF THERE IS DATA MISSING IN ANY OF THE CELLS
colSums(is.na(data_file))

#REMOVE MISSING DATA
data_file=na.omit(data_file)

# 9.A: WHICH PREDICTORS ARE QUANTITATIVE, AND WHICH ARE QUALITATIVE
names(data_file)

# THE FOLLOWING ARE QUALITATIVE
data_file$cylinders = as.factor(cylinders)
data_file$year = as.factor(year)
data_file$origin = as.factor(origin)

#THE REST ARE QUANTITATIVE
# mpg, displacement, horsepower, weight, acceleration
data_file$horsepower = as.numeric(horsepower)

# 9.B: QUANTITATIVE PREDICTORS: RANGE
print("MPG:");          range(data_file$mpg)
print("Displacement:"); range(data_file$displacement)
print("Hoursepower:");  range(data_file$horsepower)
print("Weight:");       range(data_file$weight)
print("Acceleration:"); range(data_file$acceleration)

# 9.C: QUANTITATIVE PREDICTORS: MEAN AND STANDARD DEVIATION
print("MPG:");          mean(data_file$mpg); sd(data_file$mpg)
print("Displacement:"); mean(data_file$displacement); sd(data_file$displacement)
print("Hoursepower:");  mean(data_file$horsepower); sd(data_file$horsepower)
print("Weight:");       mean(data_file$weight); sd(data_file$weight)
print("Acceleration:"); mean(data_file$acceleration); sd(data_file$acceleration)

# 9.D: Remove observation's 10-85 and reprint range, mean, and standars deviation
print("MPG:")
range(data_file$mpg[-10:-85]);  mean(data_file$mpg[-10:-85]); sd(data_file$mpg[-10:-85])
print("Displacement:")
range(data_file$displacement[-10:-85]); mean(data_file$displacement[-10:-85]); sd(data_file$displacement[-10:-85])
print("Hoursepower:")
range(data_file$horsepower[-10:-85]); mean(data_file$horsepower[-10:-85]); sd(data_file$horsepower[-10:-85])
print("Weight:")
range(data_file$weight[-10:-85]); mean(data_file$weight[-10:-85]); sd(data_file$weight[-10:-85])
print("Acceleration:")
range(data_file$acceleration[-10:-85]); mean(data_file$acceleration[-10:-85]); sd(data_file$acceleration[-10:-85])

# 9.E: DATA DISCOVERY, USE GRAPHS AND COMMENT ON FINDINGS
summary(data_file)

png("9_E(1).png")
pairs(data.frame(mpg, horsepower, cylinders, weight))
dev.off()

png("9_E(2).png")
plot(weight, mpg, ylab = "Miles per Gallon (M/g)", main = "Miles per gallon over weight")
dev.off()

png("9_E(3).png")
plot(cylinders, weight)
dev.off()

png("9_E(4).png")
plot(weight, horsepower)
dev.off()

png("9_E(5).png")
plot(cylinders, mpg)
dev.off()

png("9_E(6).png")
plot(cylinders, horsepower)
dev.off()

png("9_E(7).png")
plot(mpg, horsepower)
dev.off()

summary(data.frame(mpg, horsepower, cylinders))

cor(data.frame(weight, mpg))

# 9.F: Do your plots suggest that any of the other variables might be useful in predicting mpg? Wht (not)?

# Based on the graphs plotted above it seems that there is a 
# negative relationship between weight and miles per gallon, this assumption 
# is supported by the strong negative correlation of -0.8317389 and the graph titled:
# "Miles per gallon over weight"



# 10: USING THE BOSTON HOUSING DATA
# 10: SETUP
rm(list = ls())
dev.off()

# 10.A: LOAD BOSTON DATA SET
library(MASS)
dim(Boston)
colnames(Boston)
?Boston

#DATA PREP
data_table = Boston
attach(data_table)

#QUANTITATIVE COLUMNS:
#crim, zn, indus, nox, rm, age, dis, tax, ptratio, black, lstat, medv

#QUALITATIVE COLUMNS
data_table$chas = as.factor(chas)
data_table$rad = as.factor(rad)

# 10.B: MAKE SOME PAIRWISE SCATTERPLOTS OF THE PREDICTORS. WHAT ARE YOUR FINDINGS?
png("10_b(1).png")
scater_plot_matrix = pairs(c(data_table[,1:3],data_table[,5:8],data_table[,10:14]))
dev.off()

png("10_b(2).png")
plot(Boston$rm, Boston$medv, 
     xlab = "average number of rooms per dwelling", 
     ylab = "median value of owner-occupied homes in $1000s")
dev.off()

png("10_b(3).png")
plot(Boston$lstat, Boston$medv, 
     xlab = "lower status of the population (percent)", 
     ylab = "median value of owner-occupied homes in $1000s")
dev.off()

png("10_b(4).png")
plot(Boston$rm, Boston$lstat, 
     xlab = "average number of rooms per dwelling", 
     ylab = "lower status of the population (percent)")
dev.off()

# 10.C: WHAT PREDICTORS ARE ASSOCIATED WITH THE PERCAPITA CRIME RATE?


# 10.D: IN THE SUBURBS WHAT ARE THE HIGHEST: CRIME RATES? TAX RATES? PUPIL-TEACHER RATIOS?
# COMMENTS
summary(crim)
png("10_d(1).png")
plot(crim)
dev.off()

summary(tax)
png("10_d(2).png")
plot(tax)
dev.off()

summary(ptratio)
png("10_d(3).png")
plot(ptratio)
dev.off()

# 10.E: HOW MANY LOCATIONS ARE BOUND BY THE CHARLES RIVER?
summary(data_table$chas)

# 10.F: MEDIAN PUPIL-TEACHER RATIO AMONG THE TOWNS?
summary(data_table$ptratio)

# 10.G: lowest median value of owneroccupied homes? 
#Compare to other predictors and compare to the overall ranges for those predictors?

# 10.H: HOW MANY OF THE SUBURBS AVERAGE MORE THAN SEVEN ROOMS PER DWELLING? 
# EIGHT ROOMS? COMMENT ON THE EIGHT OR MORE ROOMS.
NROW(as.table(which(data_table$rm > 7)))
NROW(as.table(which(data_table$rm > 8)))



