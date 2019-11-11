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

sample_size = rbind(rep_func(data, "Brooklyn"), rep_func(data, "Bronx"), rep_func(data, "Manhattan"), rep_func(data, "Queens"), rep_func(data, "Staten Island"))
##################################################



pairs(data[sample__rows,-c(1:4, 6:8,13:14 )], lower.panel = NULL)

# PLOTTING BAR PLOTS BY BOROUGH
suppressMessages(
figure(legend_location = ) %>% 
  ly_bar(data$neighbourhood_group, color = data$room_type, data = data, position = "dodge", hover = TRUE) %>%
  theme_axis("x", major_label_orientation = 45 ) %>%
  x_axis(label = "Borough") %>%
  y_axis(label = "Number of listings")
)

# PLOTTING BAR PLOTS BY BOROUGH

suppressMessages(
  figure(legend_location = ) %>% 
    ly_bar(data$neighbourhood, data = data[ data$neighbourhood_group == "Brooklyn", ], position = "dodge", hover = TRUE) %>%
    theme_axis("x", major_label_orientation = 45 ) %>%
    x_axis(label = "Neighborhood") %>%
    y_axis(label = "Number of listings")
)

unique(data$neighbourhood_group)
nrow(data[ data$neighbourhood_group == "Manhattan"  ,  ])
nrow(data[ data$neighbourhood_group == "Brooklyn"  ,  ])
nrow(data[ data$neighbourhood_group == "Queens"  ,  ])
nrow(data[ data$neighbourhood_group == "Staten Island"  ,  ])
nrow(data[ data$neighbourhood_group == "Bronx"  ,  ])


info = data.frame(table(data[sample__rows, ]$room_type, data[sample__rows, ]$neighbourhood_group, data[sample__rows, ]$price))
colnames(info) = c("Listing_type", "Borough", "Price", "Number_of_listings")
unique(info$Listing_type)
unique(info$Borough)

info[ info$Borough == "Brooklyn" , ]
info[ info$Borough == "Bronx" , ""]
info[ info$Borough == "Manhattan" , ]
info[ info$Borough == "Queens" , ]
info[ info$Borough == "Staten Island" , ]




# g_map_key = "AIzaSyBNDq_9nG0AR8smomFdipJ2WWBC28AWbWU"
# polygons = geojsonR::Dump_From_GeoJson("neighbourhoods.geojson")
# suppressWarnings(
#   gmap( lat = mean(data$latitude), lng = ,mean(data$longitude), zoom = 11, width = 600, height = 600, map_style = gmap_style("blue_water"), api_key = g_map_key) %>%
#     
# )





