# Clear all existing variables in global environment
rm(list = ls())
# CLear plot tab and close/save any open files 
dev.off()
# Set the working dorectory to the location where the file was saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(rbokeh))
suppressMessages(library(leaflet))

# READ THE DATA
data = read.csv("listings_1.csv")
colnames(data)
attach(data)

# CLEANING THE DATA
## CHECKING FOR "NA" VALUES 
colSums(is.na(data))
#fix(data)

## ALL OBSERVATIONS WITH NO "last_review" VALUE HAVE NO REVIEWS
### SET "reviews_per_month" TO "0" FOR ALL RECORDS WITH NO REVIEWS
#### CHECK FOR ROWS WITH NA IN  "reviews_per_month" 
data[data$last_review == "", ]
#### SET "reviews_per_month" RECORDS TO 0 
data[data$number_of_reviews == "0", "reviews_per_month"] = 0
#### CHECK FOR ROWS WITH NA IN  "reviews_per_month" AGAIN TO CONFIRM VALUE ASSIGNMENT
data[data$last_review == "", ]

## CHECK THE STATISTICAL SUMMARIES OF EACH COLUM AND REMOVE EXTRANEOUS VALUES 
### CHECK RANGE OF MINIMUM NIGHTS 
#### check max range of values stored 
range(data$minimum_nights)
#### check number of records wit hminimum nights over one year
data[data$minimum_nights > 365, ]
#### OMIT ENTRIES WITH MINIMUM NIGHTS OVER 365
data = data[!(data$minimum_nights>365) , ]
### CHECK RANGE OF VALUES
range(data$minimum_nights)

# SAMPLE THE DATA
sample_size = nrow(data) / (nrow(data) * (0.05)^2 + 1 )
sample_size
sample__rows = sample(1:nrow(data), sample_size)

pairs(data[sample__rows,-c(1:4, 6:8,13:14 )], lower.panel = NULL)

bob = table(data$room_type, data$neighbourhood_group)

print(bob)
row.names(bob)

# PLOTTING BAR PLOTS BY BOROUGH
par(mfrow = c(2, 3))
barplot(bob[, "Manhattan"], main = "Manhatten" , xaxt = "n",  xlab = "")
text(1:4,srt = 45, adj = 1, labels = row.names(bob), xpd = TRUE)

barplot(bob[, "Bronx"], main = "Bronx" , xaxt = "n",  xlab = "")
text(1:4,srt = 45, adj = 1, labels = row.names(bob), xpd = TRUE)

barplot(bob[, "Brooklyn"], main = "Brooklyn" , xaxt = "n",  xlab = "")
text(1:4,srt = 45, adj = 1, labels = row.names(bob), xpd = TRUE)

barplot(bob[, "Queens"], main = "Queens" , xaxt = "n",  xlab = "")
text(1:4,srt = 45, adj = 1, labels = row.names(bob), xpd = TRUE)

barplot(bob[, "Staten Island"],main = "Staten Island" , xaxt = "n",  xlab = "")
text(1:4,srt = 45, adj = 1, labels = row.names(bob), xpd = TRUE)


# PLAYING WITH PLOTS 
####################################### 
# plotting on a map
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map
#######################################
