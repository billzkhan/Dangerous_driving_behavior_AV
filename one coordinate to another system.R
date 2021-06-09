library(sp)
library(rgdal)
convertCoordSystem <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}
coord <- data.frame(utmk.long=c(954677.6, 958869.4), utmk.lat=c(1951583, 1945669))
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"

to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
coord <- cbind(coord, convertCoordSystem(coord$utmk.long, coord$utmk.lat, from.crs, to.crs))

for(i in 1:nrow(coord)){
  p <- coord[i,]
  str <- paste("(", p$utmk.long, ",", p$utmk.lat, ") -> (", p$long, ",", p$lat, ")", sep="")
  print(str, quote=FALSE)
}