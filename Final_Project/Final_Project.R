# Clear all existing variables in global environment
rm(list = ls())
# CLear plot tab and close/save any open files 
dev.off()
# Set the working dorectory to the location where the file was saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

suppressMessages(library('rbokeh'))

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

users_all_listings = data.frame(table(airbnb_data[airbnb_data$calculated_host_listings_count > 0, c("host_id", "neighbourhood_group")]))
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
sample_split = sample(1:nrow(sample_data), 492*.75)
sample_training = sample_data[sample_split,-c(2,3,4,6,13)]
sample_testing = sample_data[-sample_split,-c(2,3,4,6,13)]

lin.reg_model = lm(price~.,sample_training)
lin.reg_prediction = predict(lin.reg_model, newdata = sample_testing)
plot(lin.reg_prediction, sample_testing[,6])
summary(lin.reg_model)
abline(0,1)
mse_lin.reg = mean((lin.reg_prediction - sample_testing[,6])^2)
mse_lin.reg
cor(lin.reg_prediction,sample_testing[,6])
summary(lin.reg_model)
sqrt(mse_lin.reg)

par(mfrow=c(2,2))
plot(lin.reg_model)

## according to residuals vs. leverage tells us
## that obs. #35681 is an outlier which affects
## the lm model. 
## Omitting it and see if the model improves.

sample_training["35681",]
lm_sample_training = sample_training[sample_training$id != 29964348,]

lin.reg_model_2 = lm(price~.,lm_sample_training)
lin.reg_prediction_2 = predict(lin.reg_model_2, newdata = sample_testing)
plot(lin.reg_prediction_2, sample_testing[,6])
summary(lin.reg_model_2)
abline(0,1)
mse_lin.reg_2 = mean((lin.reg_prediction_2 - sample_testing[,6])^2)
mse_lin.reg_2
cor(lin.reg_prediction_2,sample_testing[,6])
sqrt(mse_lin.reg_2)

## omitting the outlier improved the model!
## checking if omitting more outliers will improve further

par(mfrow=c(2,2))
plot(lin.reg_model_2)

## found that obs. #37300 might be worth to omit as well

sample_training["37300",]
lm_sample_training_3 = lm_sample_training[lm_sample_training$id != 30927844,]
lin.reg_model_3 = lm(price~.,lm_sample_training_3)
lin.reg_prediction_3 = predict(lin.reg_model_3, newdata = sample_testing)
plot(lin.reg_prediction_3, sample_testing[,6])
summary(lin.reg_model_3)
abline(0,1)
mse_lin.reg_3 = mean((lin.reg_prediction_3 - sample_testing[,6])^2)
mse_lin.reg_3
cor(lin.reg_prediction_3,sample_testing[,6])
sqrt(mse_lin.reg_2)

par(mfrow=c(2,2))
plot(lin.reg_model_3)

## omitting the 2nd outlier improved the model further!
## no more outliers that affect the model.


### NOTE: RUN REGRESSION TREE'S 
library(tree)
tree_airbnb = tree(price~., sample_training)
summary(tree_airbnb)
par(mfrow=c(1,1))
plot(tree_airbnb)
text(tree_airbnb)

cv_airbnb = cv.tree(tree_airbnb)
summary(cv_airbnb)

tree_prediction = predict(tree_airbnb, newdata = sample_testing)
plot(tree_prediction, sample_testing[,6])
abline(0,1)
mse_tree = mean((tree_prediction - sample_testing[,6])^2)
mse_tree
cor(lin.reg_prediction_2,sample_testing[,6])
sqrt(mse_lin.reg_2)

# QUESITON FOUR:  (LDA/QDA)
## Predict the most expensive type of listing
## NOTE: CURRENT DETERMINANT TOP 10%
### DEFINE COMPARISON

airbnb_data["Pricy_01"] = 0

private_room = airbnb_data[airbnb_data$room_type == "Private room", ]
priv_rm = quantile(private_room$price, prob=1- 10 / 100)
private_room[ which( private_room$price > priv_rm ) , "Pricy_01"] = 1


home_apt = airbnb_data[airbnb_data$room_type == "Entire home/apt", ]
ent_apt = quantile(home_apt$price , prob=1- 10 / 100)
home_apt[ which( home_apt$price > priv_rm ) , "Pricy_01"] = 1


shared_room = airbnb_data[airbnb_data$room_type == "Shared room", ]
shrd_rm = quantile(shared_room$price , prob=1- 10 / 100)
shared_room[ which( shared_room$price > priv_rm ) , "Pricy_01"] = 1


hotel_room = airbnb_data[airbnb_data$room_type == "Hotel room", ]
htl_rm = quantile( hotel_room$price , prob=1- 10 / 100)
hotel_room[ which( hotel_room$price > priv_rm ) , "Pricy_01"] = 1

dummy = MASS::qda(hotel_room$Pricy_01~., data = hotel_room)

# QUESITON FIVE: (ON HOLD)  
## Is  there a the relationship between the number 
## of reviews and borough? In which borough's are guests 
## most likely to leave a review?

### NOTE: DO SOMETHING NEW 


graphics.off()
bb=lm(minimum_nights~availability_365,sample_data)
summary(bb)
cor(sample_data$minimum_nights,sample_data$availability_365 )
coef(bb)

plot(sample_data$availability_365,sample_data$minimum_nights)

abline(bb,col="red")

