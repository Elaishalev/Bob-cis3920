###### Set Up ######
# Clear all existing variables in global environment
rm(list = ls())
# CLear plot tab and close/save any open files 
dev.off()
# Set the working dorectory to the location where the file was saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

delete_me = airbnb_data[FALSE, ]


suppressMessages(library('rbokeh'))
suppressMessages(library('plyr'))

# READ THE DATA
airbnb_data = read.csv("listings_summary.csv")
colnames(airbnb_data)
row.names(airbnb_data)
suppressMessages(attach(airbnb_data))

# CLEANING THE DATA
## CHECKING FOR "NA" VALUES 
colSums(is.na(airbnb_data))
#fix(airbnb_data)

## ALL OBSERVATIONS WITH NO "last_review" VALUE HAVE NO REVIEWS
### SET "reviews_per_month" TO "0" FOR ALL RECORDS WITH NO REVIEWS
#### CHECK FOR ROWS WITH NA IN  "reviews_per_month" 

airbnb_data[airbnb_data$last_review == "", c("id","number_of_reviews","last_review","reviews_per_month")]
# COMMENTS: These listing have an "NA" value in the reviews per month column because 
# they don't have any reviews at all. 

#### SET "reviews_per_month" RECORDS TO 0 
airbnb_data[airbnb_data$number_of_reviews == "0", "reviews_per_month"] = 0

#### CHECK FOR ROWS WITH NA IN  "reviews_per_month" AGAIN TO CONFIRM VALUE ASSIGNMENT
airbnb_data[airbnb_data$last_review == "", c("id","number_of_reviews","last_review","reviews_per_month")]



## CHECK THE STATISTICAL SUMMARIES OF EACH COLUM AND REMOVE EXTRANEOUS VALUES 
### CHECK RANGE OF MINIMUM NIGHTS 
#### check max range of values stored 
range(airbnb_data$minimum_nights)

#### check number of records wit hminimum nights over one year
airbnb_data[airbnb_data$minimum_nights > 365, c("id", "host_id", "minimum_nights")]
nrow(airbnb_data[airbnb_data$minimum_nights > 365, ])
# COMMENT: There are not a substantial number of records that are over 365
# for their minimum nights so they will be ommited

#### OMIT ENTRIES WITH MINIMUM NIGHTS OVER 365
airbnb_data = airbnb_data[!(airbnb_data$minimum_nights>365) , ]
### CHECK RANGE OF VALUES TO CONFIRM OBSERVATIONS WERE OMITED
range(airbnb_data$minimum_nights)

# ELAI CARRIED THIS FUCKING TEAM
##################################################
rep_func = function(data, borough){
  listings = data[data$neighbourhood_group== borough,]
  entire = listings[listings$room_type=="Entire home/apt",]
  hotel = listings[listings$room_type=="Hotel room",]
  private = listings[listings$room_type=="Private room",]
  shared = listings[listings$room_type=="Shared room",]
  set.seed(666) # because this is the devils work
  sample = rbind(
    entire[sample(1:nrow(entire), ceiling(nrow(entire)*.01)), ],
    hotel[sample(1:nrow(hotel), ceiling(nrow(hotel)*.01)), ],
    private[sample(1:nrow(private), ceiling(nrow(private)*.01)), ],
    shared[sample(1:nrow(shared), ceiling(nrow(shared)*.01)), ]
  )  
  rm(listings, entire, hotel, private, shared)
  return(sample)
} 

sample_data = rbind(
  rep_func(airbnb_data, "Brooklyn"), 
  rep_func(airbnb_data, "Bronx"), 
  rep_func(airbnb_data, "Manhattan"), 
  rep_func(airbnb_data, "Queens"), 
  rep_func(airbnb_data, "Staten Island")
  )
##################################################




# QUESITON ONE (Visual data exploration)
## WHAT IS THE DISTRIBUTION OF DIFFERENT 
## LISTING TYPES IN EACH BOROUGH?

### NOTE: MOVE THE LEGEND OF THE PLOT

### PLOTTING LISTING TYPES BY BOROUGH 
suppressMessages(
  figure(legend_location = ) %>% 
    ly_bar(airbnb_data$neighbourhood_group, color = airbnb_data$room_type, data = airbnb_data, position = "dodge", hover = TRUE) %>%
    theme_axis("x", major_label_orientation = 45 ) %>%
    x_axis(label = "Borough") %>%
    y_axis(label = "Number of listings")
)

### GOEPLOTING 
g_map_key = "AIzaSyBNDq_9nG0AR8smomFdipJ2WWBC28AWbWU"
polygons = geojsonR::Dump_From_GeoJson("neighbourhoods.geojson")
suppressWarnings(
  gmap( lat = mean(airbnb_data$latitude), lng = mean(airbnb_data$longitude), zoom = 11,
        width = 600, height = 600, map_style = gmap_style("blue_water"), api_key = g_map_key)
)

### NOTE: ADDITIONAL PLOTS
## SHOW THE HIGHEST AND LOWEST BY NEIGHBORHOOD (TOP 3)



# QUESITON TWO: (Summary and conclusions)
## Which boroughs  have the 
## most hosts with multiple listings? 
users_all_listings_digin = data.frame(table(airbnb_data[airbnb_data$calculated_host_listings_count > 0, c("neighbourhood_group","neighbourhood")]))
users_all_listings_digin = users_all_listings_digin[users_all_listings_digin$Freq > 0,]
users_all_listings_digin[1:8,]

borough=c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")

fuckthis=users_all_listings_digin[FALSE,]


for (i in borough) {
  bob=users_all_listings_digin[ which(users_all_listings_digin$neighbourhood_group == i & users_all_listings_digin$Freq == max(users_all_listings_digin[users_all_listings_digin$neighbourhood_group == i,"Freq"])) , ]
  fuckthis=rbind(fuckthis,bob)
}
#fuckthis is a table containiing list of max for each neighborghood
fuckthis [1:6,]

fuckno=users_all_listings [FALSE,]

for (j in borough){
 bob2=users_all_listings_digin[ which (users_all_listings_digin$neighbourhood_group == j ) , ]
 tail((bob2[order (bob2$Freq),]  ),3)
 fuckno=rbind(fuckno,tail((bob2[order (bob2$Freq),]  ),3))
}
#fuckno is a list of maxthree for each listing
fuckno[1:15,]


#dont touch anything below this commend!!!!!!!
users_all_listings = data.frame(table(airbnb_data[airbnb_data$calculated_host_listings_count > 0, c("neighbourhood_group", "neighbourhood")]))
users_all_listings = users_all_listings[users_all_listings$Freq > 0,]
users_all_listings[1:5,]

multiple_Bronx=nrow(users_all_listings[which(users_all_listings$neighbourhood_group == "Bronx" & users_all_listings$Freq > 1), ])
multiple_Queens=nrow(users_all_listings[which(users_all_listings$neighbourhood_group == "Queens" &users_all_listings$Freq > 1 ), ])
multiple_Brooklyn=nrow(users_all_listings[which(users_all_listings$neighbourhood_group == "Brooklyn"&users_all_listings$Freq > 1 ), ])
multiple_StatenIsland=nrow(users_all_listings[which(users_all_listings$neighbourhood_group == "Staten Island"&users_all_listings$Freq > 1 ), ])
multiple_Manhattan=nrow(users_all_listings[which(users_all_listings$neighbourhood_group == "Manhattan"&users_all_listings$Freq > 1 ), ])

all_Bronx=nrow(users_all_listings[users_all_listings$neighbourhood_group == "Bronx", ])
all_Queens=nrow(users_all_listings[users_all_listings$neighbourhood_group == "Queens", ])
all_Brooklyn=nrow(users_all_listings[users_all_listings$neighbourhood_group == "Brooklyn", ])
all_StatenIsland=nrow(users_all_listings[users_all_listings$neighbourhood_group == "Staten Island", ])
all_Manhattan=nrow(users_all_listings[users_all_listings$neighbourhood_group == "Manhattan", ])

Bronx_percent=(multiple_Bronx / all_Bronx)*100
Queens_percent=(multiple_Queens/all_Queens)*100
Brooklyn_percent=(multiple_Brooklyn/all_Brooklyn)*100
Manhattan_percent=(multiple_Manhattan/all_Manhattan)*100
StatenIsland_percent=(multiple_StatenIsland/all_StatenIsland)*100

Queens_percent
Brooklyn_percent
Manhattan_percent
StatenIsland_percent
Brooklyn_percent

x=c(Queens_percent, Brooklyn_percent, Manhattan_percent, StatenIsland_percent,Bronx_percent)
y=c(multiple_Queens, multiple_Brooklyn, multiple_Manhattan, multiple_StatenIsland, multiple_Bronx)

barplot(x,main="Percentage distribution of Multiple Listings ",
        names.arg=c("Queens", "Brooklyn", "Manhattan","StatenIsland","Bronx"))
barplot(y,main="Number of Multiple Listings ",
        names.arg=c("Queens", "Brooklyn", "Manhattan","StatenIsland","Bronx"))

### NOTE: modify to show the count of people with multiple listings rather then plotting the value 
### in the "calculated_host_listings_count" for each observation
plot (neighbourhood_group,   calculated_host_listings_count)

max(airbnb_data$calculated_host_listings_count)

users_all_listings[which(users_all_listings$Freq==max(users_all_listings$Freq)), ]

plot (neighbourhood_group=="Manhattan",calculated_host_listings_count)

graphics.off()


# QUESITON THREE:  (multiple linear regression)
## Predict the price of listings in different 
## boroughs based on other attributes?

sample_data$last_review = as.Date(sample_data$last_review)
set.seed(42069)
sample_split = sample(1:nrow(sample_data), 492*.60)
sample_training = sample_data[sample_split,-c(2,4,6,13)]
sample_testing = sample_data[-sample_split,-c(2,4,6,13)]

lin.reg_model = lm(price~.,sample_training)
lin.reg_prediction = predict(lin.reg_model, newdata = sample_testing)
plot(lin.reg_prediction, sample_testing$price)
summary(lin.reg_model)
abline(0,1)
mse_lin.reg = mean((lin.reg_prediction - sample_testing$price)^2)
mse_lin.reg
cor(lin.reg_prediction,sample_testing$price)
summary(lin.reg_model)
sqrt(mse_lin.reg)

par(mfrow=c(2,2))
plot(lin.reg_model)

## according to residuals vs. leverage graph, 
## obs. #37300 is an outlier which affects
## the lm model. 
## Omitting it and see if the model improves.

sample_training["37300",]
lm_sample_training = sample_training[sample_training$id != 30927844,]

lin.reg_model_2 = lm(price~.,lm_sample_training)
lin.reg_prediction_2 = predict(lin.reg_model_2, newdata = sample_testing)
plot(lin.reg_prediction_2, sample_testing$price)
summary(lin.reg_model_2)
abline(0,1, col = 5)
mse_lin.reg_2 = mean((lin.reg_prediction_2 - sample_testing$price)^2)
mse_lin.reg_2
cor(lin.reg_prediction_2,sample_testing$price)
sqrt(mse_lin.reg_2)

## omitting the outlier didn't improve the model.
## checking if omitting more outliers will improve it.

par(mfrow=c(2,2))
plot(lin.reg_model_2)

## found that obs. #40299 might be worth to omit as well

sample_training["40299",]
lm_sample_training_3 = lm_sample_training[lm_sample_training$id != 33513897,]

lin.reg_model_3 = lm(price~.,lm_sample_training_3)
lin.reg_prediction_3 = predict(lin.reg_model_3, newdata = sample_testing)
plot(lin.reg_prediction_3, sample_testing$price)
summary(lin.reg_model_3)
abline(0,1, col = 5)
mse_lin.reg_3 = mean((lin.reg_prediction_3 - sample_testing$price)^2)
mse_lin.reg_3
cor(lin.reg_prediction_3,sample_testing$price)
sqrt(mse_lin.reg_2)

par(mfrow=c(2,2))
plot(lin.reg_model_3)

## omitting the 2nd outlier didn't improve the model.
## the original LM was the best one.


### Running a REGRESSION TREE to see if it 
### predicts the price better than LM.

library(tree)
tree_airbnb = tree(price~., sample_training)
summary(tree_airbnb)
par(mfrow=c(1,1))
plot(tree_airbnb)
text(tree_airbnb)

cv_airbnb = cv.tree(tree_airbnb)
summary(cv_airbnb)

tree_prediction = predict(tree_airbnb, newdata = sample_testing)
plot(tree_prediction, sample_testing$price)
abline(0,1)
mse_tree = mean((tree_prediction - sample_testing$price)^2)
mse_tree
cor(lin.reg_prediction_2,sample_testing$price)
sqrt(mse_lin.reg_2)

## The regression tree model doesn't predict price
## as well as the linear regression model.


# QUESITON FIVE: (ON HOLD)  
## Is  there a the relationship between the number 
## of reviews and borough? 

##In which borough's are guests 
## most likely to leave a review?



### NOTE: DO SOMETHING NEW 

graphics.off()
bb=lm(minimum_nights~availability_365,sample_data)
summary(bb)
cor(sample_data$minimum_nights,sample_data$availability_365 )
coef(bb)

plot(sample_data$availability_365,sample_data$minimum_nights)

abline(bb,col="red")


## making a model to predict in which borough a given listing is
set.seed(88)
class_tree_split = sample(1:nrow(sample_data), nrow(sample_data)/2)
class_tree_training = sample_data[class_tree_split,]
class_tree_testing = sample_data[-class_tree_split,]

class_tree = tree(neighbourhood_group~
            room_type+price+minimum_nights+number_of_reviews+reviews_per_month
            +calculated_host_listings_count+availability_365
            , sample_data, subset = class_tree_training)
summary(class_tree)
plot(class_tree)
text(class_tree)

class_tree_pred = predict(class_tree,class_tree_testing, type = "class")
table(class_tree_pred, class_tree_testing$neighbourhood_group)
misclass_rate = 1-((59+80+7)/246)
misclass_rate

## the misclass rate is about 40%
## checking if pruning the tree will
## provide better results for the model

set.seed(00)
cv_class_tree = cv.tree(class_tree, FUN = prune.misclass)
cv_class_tree
par(mfrow = c(1,2))
plot(cv_class_tree$size, cv_class_tree$dev, type = "b")
plot(cv_class_tree$k, cv_class_tree$dev, type = "b")

prune_class_tree = prune.misclass(class_tree, best = 18)
plot(prune_class_tree)
text(prune_class_tree)

prune_tree_pred = predict(prune_class_tree, class_tree_testing, type = "class")
table(prune_tree_pred,class_tree_testing$neighbourhood_group)
misclass_rate_prune = 1-((58+77+7)/246)
misclass_rate_prune

## the misclassificateion rate actually went up
## by about 2%, so the model was performing
## better before the pruning.