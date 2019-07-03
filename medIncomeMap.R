# Brett W. 
# LIS4761 - Data Mining
# Homework: MapData--Visualizing Median Income on a Map
# Data taken from: https://www.psc.isr.umich.edu/dis/census/Features/tract2zip/MedianZIP-3.xlsx

# Necessary packages.
require(readr)
require(zipcode)
require(maps)
require(ggplot2)
require(dplyr)
require(ggmap)

# Got the data from the link above, then saved just the median income worksheet as a CSV. 
medianZip <- read_delim("/home/brett/LIS4761_Data_Mining/data/medianZip.csv", ":", escape_double = FALSE, trim_ws = TRUE)

# Turn data into a dataframe. 
mydata <- data.frame(medianZip)
names(mydata)[1] <- "zip"

# Checkout zipcode data. 
data("zipcode")

# Gain additional columns.
dfNew <- merge(mydata, zipcode, by="zip")

# Remove Hawaii and Alaska.
mydata <- dfNew %>% filter(state != "HI") %>% filter(state != "AK")

# Create the dataframe
dfSimple <- mydata %>% group_by(state) %>% summarize(meanIncome = mean(Median, na.rm=TRUE), meanPopulation = mean(Pop, na.rm=TRUE))

# Match with state names and abbreviations. 
dfSimple$stateName <- state.name[match(dfSimple$state,state.abb)]

# Convert all the state names to lowercase.
dfSimple$stateName <- sapply(dfSimple$stateName, tolower)

# Get the data on the state to be mapped. 
us <- map_data("state")

# Create a map for the average median income. 
map1 <- ggplot(dfSimple, aes(map_id = stateName))  
map1 <- map1 + geom_map(map = us, color="black", aes(fill=meanIncome)) 
map1 <- map1 + expand_limits(x = us$long, y = us$lat)
map1 <- map1 + coord_map() +  ggtitle("Average Median Income of the United States")
map1

# Create a map for the population. 
map2 <- ggplot(dfSimple, aes(map_id = stateName))  
map2 <- map2 + geom_map(map = us, color="black", aes(fill=meanPopulation)) 
map2 <- map2 + expand_limits(x = us$long, y = us$lat)
map2 <- map2 + coord_map() +  ggtitle("Population of the United States")
map2

# Create a map for income by zipcode. 
map3 <- ggplot(data=dfSimple, aes(map_id = stateName))  
map3 <- map3 + geom_map(map = us, color="white", fill="black") 
map3 <- map3 + expand_limits(x = us$long, y = us$lat)
map3 <- map3 + coord_map() +  ggtitle("Income per zip code")
map3

# Create a map for income by zipcode. 
map4 <- ggplot(data=dfSimple, aes(map_id = stateName))  
map4 <- map4 + geom_map(map = us, color="white", fill="black") 
map4 <- map4 + expand_limits(x = us$long, y = us$lat)
map4 <- map4 + coord_map() +  ggtitle("Income per zip code")
map4

# Register the Google API
register_google(key = "***")

# Create a map for income by zipcode. 
mapZip <- ggplot(data=dfSimple, aes(map_id = stateName))  
mapZip <- mapZip + geom_map(map = us, color="white", fill="black") 
mapZip <- mapZip + expand_limits(x = us$long, y = us$lat)
mapZip <- mapZip + coord_map() +  ggtitle("mapZipZoomed")
mapZip

# Use geocode function to get latitude and longtitude of NYC
latlon <- geocode("NYC, ny")

# Create the first zoomed map based on "mapZip", and plot a point representing NYC
mapZipZoomed <- mapZip + geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size = 3) 

# zoom into the region arount NYC with 10 degrees latitude and longtitude fluctuation (+/- 10)
mapZipZoomed <- mapZipZoomed + xlim(latlon$lon-10, latlon$lon+10) + ylim(latlon$lat-10,latlon$lat+10) + coord_map()

# plot the map
mapZipZoomed

# Create a map for income by zipcode. 
mapD <- ggplot(data=dfSimple, aes(map_id = stateName))  
mapD <- mapD + geom_map(map = us, color="white", fill="black") 
mapD <- mapD + expand_limits(x = us$long, y = us$lat)
mapD <- mapD + coord_map() +  ggtitle("mapDZoomed")
mapD

# create the first zoomed map based on "mapD" and plot a point, which representing NYC
mapDZoomed <- mapD + geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size = 3) 

# zoom into the region arount NYC (latitude and longtitude +/- 10)
mapDZoomed <- mapDZoomed + xlim(latlon$lon-10,latlon$lon+10) + ylim(latlon$lat-10,latlon$lat+10) + coord_map()

# plot the map
mapDZoomed






