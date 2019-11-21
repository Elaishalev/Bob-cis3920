###### Set Up ######
# Clear all existing variables in global environment
rm(list = ls())
# CLear plot tab and close/save any open files 
# dev.off()
# Set the working dorectory to the location where the file was saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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



## ELAI'S SAMPLING FUNCTION
# ELAI CARRIED THIS FUCKING TEAM
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
#### SET UP COMPLETE ####




#### QUESITON ONE ####
## TASK: Visual data exploration
## WHAT IS THE DISTRIBUTION OF DIFFERENT 
## LISTING TYPES IN EACH BOROUGH?

### NOTE: MOVE THE LEGEND OF THE PLOT

### PLOTTING LISTING TYPES BY BOROUGH 
suppressMessages(
  figure( legend_location = NULL, data = airbnb_data) 
  %>% ly_bar(neighbourhood_group, color = room_type, 
           position = "dodge", hover = TRUE) 
  %>% theme_axis("x", major_label_orientation = 45 ) 
  %>% x_axis(label = "Borough") 
  %>% y_axis(label = "Number of listings")
)

### GOEPLOTING 
g_map_key = "AIzaSyBNDq_9nG0AR8smomFdipJ2WWBC28AWbWU"
polygons = data.frame(jsonlite::fromJSON("neighbourhoods.geojson"))

suppressWarnings(
  gmap( api_key = g_map_key,
        lat = mean(airbnb_data$latitude), lng = mean(airbnb_data$longitude), 
        zoom = 11, width = 600, height = 600,
        map_style = gmap_style("blue_water"),
        tools = c( "pan", "wheel_zoom")
        ) 
  %>% ly_points( y = airbnb_data$latitude, x = airbnb_data$longitude, data = airbnb_data, color = neighbourhood_group,
               glyph = 21, size = 2, hover = T, lname = "air_bnb_locations") 
)


#### QUESITON TWO ####
## TASK: Summary and conclusions
## Which boroughs  have the 
## most hosts with multiple listings? 
listings_by_neighbourhood = data.frame(table(airbnb_data[airbnb_data$calculated_host_listings_count > 0, c("neighbourhood_group","neighbourhood")]))
listings_by_neighbourhood = listings_by_neighbourhood[listings_by_neighbourhood$Freq > 0,]
head(listings_by_neighbourhood)

#max_3 will be a table containiing list of top 3 for each neighborghood
max_3=listings_by_neighbourhood[FALSE,]

#min_3 will be a list of maxthree for each listing
min_3=listings_by_neighbourhood [FALSE,]


## FOR LOOPS TO BUILD DATA FRAME
for (i in unique(airbnb_data$neighbourhood_group ) ) {
  bob=listings_by_neighbourhood[ which (listings_by_neighbourhood$neighbourhood_group == i ) , ]
  tail((bob[order (bob$Freq),]  ),3)
  max_3=rbind(max_3,tail((bob[order (bob$Freq),]  ),3))
}

for (i in unique(airbnb_data$neighbourhood_group ) ) {
 bob=listings_by_neighbourhood[ which (listings_by_neighbourhood$neighbourhood_group == i ) , ]
 tail((bob[order (bob$Freq),]  ),3)
 min_3=rbind(min_3,tail((bob[order (bob$Freq),]  ),3))
}
# REMOVE EXCESS VARIABLES
rm(i, bob)

# PRINT RESULTS
head(max_3)
head(min_3)


#dont touch anything below this commend!!!!!!!
listing_count_by_user_id = data.frame(table(airbnb_data[airbnb_data$calculated_host_listings_count > 0, c("host_id","neighbourhood_group")]))
listing_count_by_user_id = listing_count_by_user_id[listing_count_by_user_id$Freq > 0,]
head(listing_count_by_user_id)

multi_lister_composition = data.frame( 
  borough = character(), total = numeric(),
  milti_listers = numeric(), percent_comp =  numeric()
)

suppressWarnings(for (i in c(1:5) ) {
  multi_lister_composition[i,] = list(
    unique(airbnb_data$neighbourhood_group)[i],
    nrow(listing_count_by_user_id[listing_count_by_user_id$neighbourhood_group == unique(airbnb_data$neighbourhood_group)[i], ]),
    nrow(listing_count_by_user_id[ which( listing_count_by_user_id$neighbourhood_group == unique(airbnb_data$neighbourhood_group)[i] & listing_count_by_user_id$Freq > 1), ]),0
)})
rm(i)

multi_lister_composition$borough = unique(airbnb_data$neighbourhood_group)
multi_lister_composition$percent_comp = 100*multi_lister_composition$milti_listers / multi_lister_composition$total
head(multi_lister_composition)


### PLOTTING PERCENT COMPOSITION
suppressMessages(
  figure( data =  multi_lister_composition , title = "Percent composition of users with multiple listings" ,legend_location = NULL,
          tools = c("wheel_zoom", "reset" ,"pan"), toolbar_location = "above",
          ylim = c(0,max(multi_lister_composition$percent_comp)+5) ) %>% 
    ly_bar(x=borough,y=percent_comp,color = multi_lister_composition$borough, hover = TRUE,
           lname = "percent") %>%
    x_axis(label = "Borough") %>%
    y_axis(label = "Percentage of users", 
           number_formatter = "printf", format = "%0.2f%%")
)

### PLOTTING TOTAL LISTINGS
suppressMessages(
  figure( data =  multi_lister_composition , title = "Count of users by borough" ,legend_location = NULL,
          tools = c("wheel_zoom", "reset" ,"pan"), toolbar_location = "above",
          ylim = c(0,max(multi_lister_composition$total)+500) ) %>% 
    ly_bar(x=borough,y=total, color = borough,
           hover = TRUE, position = 'dodge') %>%
    x_axis(label = "Borough") %>%
    y_axis(label = "Number of users")
)

head(multi_lister_composition)


#### QUESITON THREE ####
## TASK: MULTIPLE LINEAR REGRESSION / REGRESSION TREE
## PREDICT THE PRICE OF LISTINGS IN DIFFERENT 
## BOROUGHS ON OTHER ATTRIBUTES?

sample_data$last_review = as.Date(sample_data$last_review)
set.seed(42069)
sample_split = sample(1:nrow(sample_data), 492*.60)
sample_training = sample_data[sample_split,-c(2,4,6,13)]
sample_testing = sample_data[-sample_split,-c(2,4,6,13)]

lin.reg_model = lm(price~.,sample_training)
lin.reg_prediction = predict(lin.reg_model, newdata = sample_testing)
plot(lin.reg_prediction, sample_testing$price)
summary(lin.reg_model)
abline(0,1, col = 5)
mse_lin.reg = mean((lin.reg_prediction - sample_testing$price)^2)
mse_lin.reg
cor(lin.reg_prediction,sample_testing$price)
summary(lin.reg_model)
sqrt(mse_lin.reg)

  
par(mfrow=c(2,2))
plot(lin.reg_model)

## ACCORDING TO THE RESIDUALS VS. LEVERGAE GRAPH, 
## OBS. #37300 IS AN OUTLIER WHICH AFFECTS
## THE LM MODEL 
## OMITTING IT AND CHECKING IF THE MODEL IMPROVES.

sample_training["37300",]
lm_sample_training2 = sample_training[sample_training$id != 30927844,]

lin.reg_model_2 = lm(price~.,lm_sample_training2)
lin.reg_prediction_2 = predict(lin.reg_model_2, newdata = sample_testing)
plot(lin.reg_prediction_2, sample_testing$price)
summary(lin.reg_model_2)
abline(0,1, col = 5)
mse_lin.reg_2 = mean((lin.reg_prediction_2 - sample_testing$price)^2)
mse_lin.reg_2
cor(lin.reg_prediction_2,sample_testing$price)
sqrt(mse_lin.reg_2)

par(mfrow=c(2,2))
plot(lin.reg_model_2)

## OMITTING THE OUTLIER DIDN'T IMPROVE THE MODEL.
## WE FIND THAT OBS. #40299 MIGHT BE WORTH TO OMIT AS WELL.
## CHECKING IF OMITTING ANOTHER OUTLIER WILL IMPROVE IT.

sample_training["40299",]
lm_sample_training_3 = lm_sample_training2[lm_sample_training2$id != 33513897,]

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

## OMITTING THE 2nd OUTLIER DIDN'T IMPROVE THE MODEL.
## THE ORIGINAL LM WAS THE BEST ONE.


### RUNNING A REGRESSION TREE TO SEE IF IT 
### PREDICTS THE PRICE BETTER THAN LM.

library(tree)
library(gbm)
library(randomForest)

tree_airbnb = tree(price~., sample_training)
summary(tree_airbnb)
par(mfrow=c(1,1))
plot(tree_airbnb)
text(tree_airbnb)

cv_airbnb = cv.tree(tree_airbnb)
summary(cv_airbnb)

tree_prediction = predict(tree_airbnb, newdata = sample_testing)
plot(tree_prediction, sample_testing$price)
abline(0,1, col = 5)
mse_tree = mean((tree_prediction - sample_testing$price)^2)
mse_tree
cor(lin.reg_prediction_2,sample_testing$price)
sqrt(mse_tree)

## THE REGRESSION TREE MODEL DOESN'T PREDICT PRICE
## AS WELL AS THE LINEAR REGRESSION MODEL.


## BOOSTING THE REG. TREE IN ORDER TO TRY GET 
## A BETTER PREDICTION TREE

boost_reg_tree = gbm(price~., data = sample_training, distribution = "gaussian"
                     , n.trees = 5000)
summary(boost_reg_tree)
boost_reg_pred = predict(boost_reg_tree, newdata = sample_testing, n.trees = 5000)
mse_reg_boost = mean((boost_reg_pred - sample_testing$price)^2)
mse_reg_boost
cor(boost_reg_pred,sample_testing$price)

## BOOSTING IMPROVED THE MISCLASSIFICATION RATE!
## BUT IT'S STILL NOT AS ACCURATE AS LIN. REG..
## SUMMARY TELLS US THAT AVAILABILITY IS THE MOST 
## INFLUENCIAL ATTRIBUTE ON PREDICTING THE PRICE!

## NOW TRYING BAGGING TO COMPARE TO PREVIOUS METHODS

bagging_reg_tree = randomForest(price~., data = sample_training
                                , mtry = 11, importance = T)
bagging_reg_pred = predict(bagging_reg_tree, newdata = sample_testing)
plot(bagging_reg_pred, sample_testing$price)
abline(0,1, col = 5)
mse_bagging_reg = mean((bagging_reg_pred - sample_testing$price)^2)
mse_bagging_reg
summary(bagging_reg_tree)
cor(bagging_reg_pred,sample_testing$price)

## BAGGING HAS PROVIDED THE BEST 
## MISCLASSIFICATION RATE, AND THE BEST CORRELATION
## BETWEEN PREDICTIONS AND TESTING DATA!



#### QUESITON FOUR ####
## IS THERE A RELATIONSHIP BETWEEN THE NUMBER OF 
## REVIEWS AND THE DIFFERENT OROUGHS?

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


## QUESTION FIVE ##
## CAN WE PREDICT IN WHICH BOROUGH A GIVEN LIST IS
## WITHOUT KNOWING IT'S COORDINATES AND NEIGHBORHOOD?

set.seed(88)
class_tree_split = sample(1:nrow(sample_data), nrow(sample_data)*.5)
class_tree_training = sample_data[class_tree_split,]
class_tree_testing = sample_data[-class_tree_split,]

class_tree = tree(neighbourhood_group~
            room_type+price+minimum_nights+number_of_reviews+reviews_per_month
            +calculated_host_listings_count+availability_365
            , class_tree_training)
summary(class_tree)
plot(class_tree)
text(class_tree)

class_tree_pred = predict(class_tree,class_tree_testing, type = "class")
table(class_tree_pred, class_tree_testing$neighbourhood_group)
misclass_rate = 1-((67+83+5)/246)
misclass_rate

## THE MISCLASSIFICATION RATE IS ABOUT 37%.
## CHECKING IF PRUNING THE TREE WILL PROVIDE
## BETTER RESULTS FOR THE TREE MODEL

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
misclass_rate_prune = 1-((47+81+3)/246)
misclass_rate_prune

## THE MISCLASSIFICATION RATE WENT UP BY ALMOST 10%.
## THE TREE MODEL WAS PERFORMING BETTER BEFORE THE PRUNING.

## BOOSTING THE TREE TO TRY AND IMPROVE IT
# tried, but it didn't work 

## Trying bagging...




## WE CAN LEARN THAT THE DATA OF LISTINGS IN MANHATTAN AND
## BROOKLYN IS VERY SIMILAR TO EACH OTHER, AND IS VERY
## DIFFERENT FROM THE DATA OF LISTINGS IN THE OTHER BOROUGHS.