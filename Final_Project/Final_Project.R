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

# ELAI IS WORKING ON SAMPLE THE DATA
##################################################
rep_func = function(data, borough){
  listings = data[data$neighbourhood_group== borough,]
  entire = listings[listings$room_type=="Entire home/apt",]
  hotel = listings[listings$room_type=="Hotel room",]
  private = listings[listings$room_type=="Private room",]
  shared = listings[listings$room_type=="Shared room",]
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



# QUESITON TWO: (Summary and conclusions)
## Which boroughs  have the 
## most hosts with multiple listings? 

users_all_listings = data.frame(
  table(
    airbnb_data[airbnb_data$calculated_host_listings_count > 0, c("host_id", "neighbourhood_group")]
  )
)
users_all_listings = users_all_listings[users_all_listings$Freq > 0,]
users_all_listings[1:5,]

multiple_Bronx=nrow( 
  users_all_listings[
    which(users_all_listings$neighbourhood_group == "Bronx" & users_all_listings$Freq > 1)
                     , ]
  )
multiple_Bronx

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
par(mfrow=c(2,2))

x=c(Queens_percent, Brooklyn_percent, Manhattan_percent, StatenIsland_percent, 
    Bronx_percent)

barplot(x,main="Percentage distribution of Multiple Listings ",
        names.arg=c("Queens", "Brooklyn", "Manhattan","StatenIsland","Bronx"))

y=c(multiple_Queens, multiple_Brooklyn, multiple_Manhattan, multiple_StatenIsland, multiple_Bronx)
barplot(y,main="Number of Multiple Listings ",
        names.arg=c("Queens", "Brooklyn", "Manhattan","StatenIsland","Bronx"))


plot (neighbourhood_group,   calculated_host_listings_count)

max(airbnb_data$calculated_host_listings_count)
users_all_listings[which(users_all_listings$Freq==max(users_all_listings$Freq)), ]

plot (neighbourhood_group=="Manhattan",calculated_host_listings_count)

graphics.off()


# QUESITON THREE:  (multiple linear regression)
## Predict the price of listings in different 
## boroughs based on other attributes?

as.Date(sample_data$last_review)
set.seed(323)
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
plot(density(lin.reg_prediction))
cor(lin.reg_prediction,sample_testing[,6])
summary(lin.reg_model)
sqrt(mse_lin.reg)

par(mfrow=c(2,2))
plot(lin.reg_model)

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

qda_1 = 

# QUESITON FIVE:
## Is  there a the relationship between minimum 
## number of  nights   and apartment availability 
## during calendar year? 

graphics.off()
bb=lm(minimum_nights~availability_365,sample_size)
summary(bb)
cor(sample_size$minimum_nights,sample_size$availability_365 )
coef(bb)

plot(sample_size$availability_365,sample_size$minimum_nights)

abline(bb,col="red")
