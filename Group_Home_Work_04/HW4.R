rm(list = ls())
dev.off()

nba = read.csv("CIS_HW4.csv", encoding = "lat")
nba
names(nba)
nba = as.matrix(nba[,-1])
names(nba)
is.na(nba)
colSums(is.na(nba))
fg._na = nba[ rowSums( is.na(nba) ) > 0, ]
colSums(is.na( fg._na))

summary(fg._na)
fix(fg._na)


# ALL NA VALUES IN THE 3P% COL CAN BE ASSIGNED TO "0"
# IF THE 3PA COL IS ALSO "0"

# ALL "FG%" WITH NA VALUES CAN BE SET TO "0" IF THE FG VALUE IS 
# AT LEAST "0"

# WE CAN OMIT ALL "EFG%" WITH NA VALUES BECAUSE THEY ALL 
# HAVE TOO LITTLE PLAY TIME TO EVEN MAKE A SHOT

# FT. CAN BE SET TO 0 IF FT IS AT LEAST 0

# omit all observations with less then 6 minutes of play time

nba = nba[nba$MP>6,]
colSums(is.na(nba))

fg._na[,is.na(fg._na)] = 0
fix(fg._na)
fg._na[colSums(is.na(fg._na)) > 0,"x3P."] = 0

elai = function(x){
  for (index in 1:length(x)) {
    if (is.na(x[index])  ) {
      print("hi")
      print(index)
      x[NA] = 0
    }  
  }
}

for (l in 1:length(comments)) {
  if (!is.na(comments[l])) print(comments[l])
}



  
elai(fg._na$FT.)
elai(fg._na$X3P.)


fix(fg._na)
