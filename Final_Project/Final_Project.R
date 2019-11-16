# Clear all existing variables in global environment
rm(list = ls())
# CLear plot tab and close/save any open files 
dev.off()
# Set the working dorectory to the location where the file was saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

suppressMessages(library(rbokeh))

# READ THE DATA
data = read.csv("listings_summary.csv")
colnames(data)
suppressMessages(attach(data))

# CLEANING THE DATA
## CHECKING FOR "NA" VALUES 
colSums(is.na(data))
#fix(data)

## ALL OBSERVATIONS WITH NO "last_review" VALUE HAVE NO REVIEWS
### SET "reviews_per_month" TO "0" FOR ALL RECORDS WITH NO REVIEWS
#### CHECK FOR ROWS WITH NA IN  "reviews_per_month" 
data[data$last_review == "", c("id","number_of_reviews","last_review","reviews_per_month")]
# COMMENTS: These listing have an "NA" value in the reviews per month column because 
# they don't have any reviews at all. 

#### SET "reviews_per_month" RECORDS TO 0 
data[data$number_of_reviews == "0", "reviews_per_month"] = 0

#### CHECK FOR ROWS WITH NA IN  "reviews_per_month" AGAIN TO CONFIRM VALUE ASSIGNMENT
data[data$last_review == "", c("id","number_of_reviews","last_review","reviews_per_month")]



## CHECK THE STATISTICAL SUMMARIES OF EACH COLUM AND REMOVE EXTRANEOUS VALUES 
### CHECK RANGE OF MINIMUM NIGHTS 
#### check max range of values stored 
range(data$minimum_nights)

#### check number of records wit hminimum nights over one year
data[data$minimum_nights > 365, c("id", "host_id", "minimum_nights")]
nrow(data[data$minimum_nights > 365, ])
# COMMENT: There are not a substantial number of records that are over 365
# for their minimum nights so they will be ommited

#### OMIT ENTRIES WITH MINIMUM NIGHTS OVER 365
data = data[!(data$minimum_nights>365) , ]
### CHECK RANGE OF VALUES TO CONFIRM OBSERVATIONS WERE OMITED
range(data$minimum_nights)

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

sample_size = rbind(
  rep_func(data, "Brooklyn"), 
  rep_func(data, "Bronx"), 
  rep_func(data, "Manhattan"), 
  rep_func(data, "Queens"), 
  rep_func(data, "Staten Island")
  )
##################################################

# QUESITON ONE (Visual data exploration)
## WHAT IS THE DISTRIBUTION OF DIFFERENT 
## LISTING TYPES IN EACH BOROUGH?

### PLOTTING LISTING TYPES BY BOROUGH 
suppressMessages(
  figure(legend_location = ) %>% 
    ly_bar(data$neighbourhood_group, color = data$room_type, data = data, position = "dodge", hover = TRUE) %>%
    theme_axis("x", major_label_orientation = 45 ) %>%
    x_axis(label = "Borough") %>%
    y_axis(label = "Number of listings")
)

### GOEPLOTING 
g_map_key = "AIzaSyBNDq_9nG0AR8smomFdipJ2WWBC28AWbWU"
polygons = geojsonR::Dump_From_GeoJson("neighbourhoods.geojson")
suppressWarnings(
  gmap( lat = mean(data$latitude), lng = mean(data$longitude), zoom = 11, width = 600, height = 600, map_style = gmap_style("blue_water"), api_key = g_map_key)
)



# QUESITON TWO: (Summary and conclusions)
## Which boroughs and neighborhoods have the 
## most hosts with multiple listings? 



# QUESITON THREE:  (multiple linear regression)
## Predict the price of listings in different 
## boroughs based on other attributes?

as.Date(sample_size$last_review)
set.seed(323)
sample_split = sample(1:nrow(sample_size), 492*.75)
sample_training = sample_size[sample_split,-c(2,3,4,6,13)]
sample_testing = sample_size[-sample_split,-c(2,3,4,6,13)]

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
## Predict the most expensive type of listing?

# NOTE: CURRENT DETERMINANT TOP 10%


### ADD DETERMINANT DATA SERIES TO THE TABLE
### THIS WILL BE DEFINED AS THE TOP __% OF PRICES 
### OF EACH LISTING TYPE  
data["Pricy_01"] = 0

unique(data$neighbourhood_group)
unique(data$room_type)

# DEFINE MANHATTAN
summary(data[ which( data$neighbourhood_group == "Manhattan"), "price" ])
summary(data[ which( data$neighbourhood_group == "Manhattan"), "price" ])
summary(data[ which( data$neighbourhood_group == "Manhattan"), "price" ])
summary(data[ which( data$neighbourhood_group == "Manhattan"), "price" ])

# DEFINE BROOKLYN
summary(data[ which( data$neighbourhood_group == "Brooklyn"), "price" ])
summary(data[ which( data$neighbourhood_group == "Brooklyn"), "price" ])
summary(data[ which( data$neighbourhood_group == "Brooklyn"), "price" ])
summary(data[ which( data$neighbourhood_group == "Brooklyn"), "price" ])

# DEFINE QUEENS
summary(data[ which( data$neighbourhood_group == "Queens"), "price" ])
summary(data[ which( data$neighbourhood_group == "Queens"), "price" ])
summary(data[ which( data$neighbourhood_group == "Queens"), "price" ])
summary(data[ which( data$neighbourhood_group == "Queens"), "price" ])


# DEFINE STATEN ISLAND
summary(data[ which( data$neighbourhood_group == "Staten Island"), "price" ])
summary(data[ which( data$neighbourhood_group == "Staten Island"), "price" ])
summary(data[ which( data$neighbourhood_group == "Staten Island"), "price" ])
summary(data[ which( data$neighbourhood_group == "Staten Island"), "price" ])

# DEFINE BRONX
summary(data[ which( data$neighbourhood_group == "Bronx"), "price" ])
summary(data[ which( data$neighbourhood_group == "Bronx"), "price" ])
summary(data[ which( data$neighbourhood_group == "Bronx"), "price" ])
summary(data[ which( data$neighbourhood_group == "Bronx"), "price" ])

y_formula = "Pricy_01 ~ "

# QUESITON FIVE:
## Is  there a the relationship between minimum 
## number of  nights   and apartment availability 
## during calendar year? 

cor(sample_size$minimum_nights, sample_size$availability_365)





