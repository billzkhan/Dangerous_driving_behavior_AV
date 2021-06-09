#install.packages("beepr")
library(beepr)
#loading trip data

trip_data <- read.csv("D:/BILAL/CarInfo10_trip.csv",sep = ',', head = TRUE)
trip_data<- trip_data[,c(1,5)]
df <- read.csv("D:/BILAL/data_copy.csv",sep=",", head=TRUE)


#renaming the column names
names(df)<- c("trip_id", "D", "CD", "v", "RPM", "break","long","lat", "angle", "Vx","Vy", "Status_code", "Date_time","peak_time", "delta_v")
names(trip_data)<- c("trip_id","car_type")
trip_data<- trip_data[1:200,]
#vlookup in r to add cartype into original data
data <- (merge(df, trip_data, by = 'trip_id', all.x = TRUE))
data<- data[1:20,]
data$longitude<- data$long/1000000
data$latitude<- data$lat/1000000
data<- data[,c('trip_id','longitude','latitude')]
#try code for location now.................................................................

library(ggmap)

register_google(key = 'AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU')
#install.packages("googleway")
library(googleway)

#try1........................
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origAddress <- data

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$addresses[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}
# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "geocoded.csv", row.names=FALSE)
geocoded<- data.frame()

for (i in nrow(data)){
  df[i] <- google_reverse_geocode(location = data,
                               result_type = c("street_address", "postal_code"),
                               location_type = "rooftop",
                               key = "AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU",
                               simplify = TRUE, language = "ko")
  
  df$results$address_components
  df$results$formatted_address
}

dff<- google_reverse_geocode(location = c(as.numeric(data["latitude"]),as.numeric(data["longitude"])),
                       result_type = c("street_address", "postal_code"),
                       location_type = "rooftop",
                       key = "AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU",
                       simplify = TRUE, language = "ko")

df$results$address_components
df$results$formatted_address

#try2..............................................
df_path <- read.table(text = "lat lon
36.49663 127.2573
36.49702 127.2575
36.49752 127.2578", header = T, stringsAsFactors = F)

res <- google_snapToRoads(df_path = df_path, key = key)
res$snappedPoints

#try3......................................
library(revgeo)
rev<- revgeo(longitude=127.2573, latitude=36.49663, provider = 'google', API = key, output = NULL,
       item = 'street')

revgeo(127.2573,36.49663)

write.csv(data, "D:data.csv")






