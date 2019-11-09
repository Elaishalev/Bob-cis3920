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
data = read.csv("AB_NYC_2019.csv")
colnames(data)
attach(data)

# CLEANING THE DATA

# CHECKING FOR "NA" VALUES 
colSums(is.na(data))
fix(data)
data = na.omit(data)
colSums(is.na(data))

# CHECK THE STATISTICAL SUMMARIES OF EACH COLUM AND REMOVE EXTRANEOUS VALUES 

## CHECK RANGE OF MINIMUM NIGHTS 
### check max range of values stored 
range(data$minimum_nights)
### check number of records wit hminimum nights over one year
data[data$minimum_nights > 365, ]
data = data[!(data$minimum_nights>365) , ]
range(data$minimum_nights)



## CHECK PRICE RANGE OF "price" COLUMN
range(data$price)
### CHECK ROWS FOR ANY WEIRD VALUES
data[ !(data$price == 0)  , ]
### OMIT RECORDS WITH A PRICE OF $0.00
data = data[ !(data$price == 0)  , ]


# plotting on a map
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map


