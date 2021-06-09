data<- read.csv("D:/BILAL/output_taxi.csv")
# data<- data[1:2000,]
data$long<- data$long/1000000
data$lat<- data$lat/1000000
data<- data[1:500,]

#install.packages("readxl")
library(readxl)
route<- read_excel("D:/BILAL/TB_ROAD.xlsx")
route<- route[route$DRCT_CD==0,]
x<- unique(route$ROAD_NAME) #46 road names

route_1<- route[route$ROAD_NAME=="일반국도1호선",]
route_1<- route_1[1:9,]
v_lines<- points_to_line(route_1, long = "From_x", lat = "From_y", id_field = NULL, sort_field = NULL)
library(leaflet)
leaflet(data = v_lines) %>%
  addTiles() %>%
  addPolylines()
#install.packages("geosphere")
library(geosphere)
#install.packages("raster")
library(raster)
# NOT RUN {
line_from <- cbind(route$From_x, route$From_y)
line_from<- line_from[1:10,]
line_to <- cbind(route$To_x, route$To_y)
B_point<- cbind(data$long,data$lat)
# d <- dist2Line(line_from, line_to)
# d<- distm(line_from,line_to, fun = distGeo)
# d<- pointDistance(line_from,line_to, lonlat = TRUE)

d<- pointDistance(B_point, line_from, lonlat = TRUE)
plot(makeLine(line_from), type='l')
points(line)
points(B_point, col='blue', pch=20)
points(d[,2], d[,3], col='red', pch='x')

for(i in nrow(data)){
  for (j in nrow(route_1)){
    x<- pointDistance(B_point,line_from, lonlat = TRUE)
    }
}



points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}



























#install.packages("googleway")
library(googleway)

key <- "AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU"

#try
near_roads<- google_nearestRoads(data, lat = "lat", lon = "long", key = key)



#try

res <- apply(data, 1, function(x){
  google_reverse_geocode(location = c(x["latitude"], x["longitude"]),
                         result_type = c("street_address", "postal_code"),
                         location_type = "rooftop",
                         key = "AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU",
                         simplify = TRUE, language = "ko")
})

## Everything contained in 'res' is all the data returnd from Google Maps API
## for example, the geometry section of the first lat/lon coordiantes

for (i in 1:nrow(data)){
  x[i]<- unlist(res[[i]]$results$formatted_address)
  }
address<- unlist(x)
data<- cbind(data,address)

write.csv(data,"D:/address.csv")


