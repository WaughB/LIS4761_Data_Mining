# Brett W. 
# LIS4761 - Data Mining
# Lab 2: Map Data and Visualization

# Install necessary packages. 
require(ggplot2)
require(ggmap)
require(readr)

# Load in data. 
crimeInSYR <- read_csv("/home/brett/LIS4761_Data_Mining/data/crimeInSYR.csv", col_names = FALSE)

# Create proper column names. 
colnames(crimeInSYR)[1] <- "type"
colnames(crimeInSYR)[2] <- "address"
colnames(crimeInSYR)[3] <- "city"
colnames(crimeInSYR)[4] <- "date"

# Register Google API key. 
# register_google(key = "***")

# Create address_complete
crimeInSYR$address_complete <- paste(crimeInSYR$address, crimeInSYR$city)

# Send address to Google and record the latitudes and longitudes. 
latlon <- geocode(crimeInSYR$address_complete)

# Get latitude and longtitude for Syracuse University
syr <-geocode ("syracuse university, syracuse, ny")
syr

# Obtain maps from multiple sources and zoom into the region around Syracuse University
syr.map <- get_map(location=syr, zoom=11)

# Generate map and sore it in "mapSimple"
mapSimple <- ggmap(syr.map)

# Plot the map
mapSimple

# Create the map with the crime points on it
pointMap <- mapSimple + geom_point(data=latlon, aes(x= latlon$lon, y= latlon$lat), alpha=0.5, size=3, color="blue")
pointMap

# Density map
densityMap <- mapSimple + geom_density2d(data=latlon, aes(x= latlon$lon, y= latlon$lat))
densityMap
