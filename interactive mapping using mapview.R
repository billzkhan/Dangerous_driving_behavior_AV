library(data.table)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(sf)
#install.packages("mapview")
library(mapview)
# install.packages("maps")
library(maps)
library(sf)
library(dplyr)
library(stringr)
library(sp)


data<- read.csv("D:/BILAL/output_taxi.csv")
# data<- data[1:2000,]
data$long<- data$long/1000000
data$lat<- data$lat/1000000
data<- data[1:20000,]

#plotting the given lat and long of data
chi_dat<- as.data.table(data)
coordinates(chi_dat)<- c("long","lat")
crs.geo1<- CRS("+proj=longlat")
proj4string(chi_dat) = crs.geo1
# plot(chi_dat, pch=20, col="steelblue")

#making map of sejong city using google map
#install.packages("tidyverse")

register_google(key = 'AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU')

locations_sf<- st_as_sf(data, coords = c("long","lat"), crs=4326)
#view based on continuous variable
mapview(locations_sf, zcol = "Diff_v", highlight = TRUE)

#view based on categorical variable defining colors of each factor
clrs = c("blue","red")
mapview(locations_sf, zcol = "Ra", highlight = TRUE, col.regions = clrs)

#turn on all columns of the supplied data
mapview(locations_sf, burst = TRUE, hide = TRUE, zcol = "Ra", grid = TRUE)

#viewextent
viewExtent(locations_sf)
mapview(st_bbox(locations_sf))
viewExtent(locations_sf)+mapview(locations_sf, zcol="Diff_v")


#making map using shape file
# install.packages("sf")
library(sf)
library(dplyr)
library(stringr)
library(data.table)
library(sp)

shp<- st_read(file.choose())
plot(shp)
#iconv is for encoding
head(shp)

Encoding(shp$SIG_KOR_NM)<- "UTF-8"
shp$SIG_KOR_NM<- iconv(shp$SIG_KOR_NM,
                       from = "cp949",
                       to= "utf-8",
                       sub=NA,
                       mark = TRUE,
                       toRaw = FALSE) # not working

shp_spatial<- as_Spatial(shp)

library(ggplot2)
df<- fortify(shp_spatial)
class(df)
head(df)

df %>%
  ggplot(aes(x=long, y= lat, group= group))+
  geom_path(color="black")


shp_spatial@data$id<- rownames(x=shp_spatial@data)
df_all<- left_join(df,shp_spatial@data, by = "id")

df_all %>% 
  filter(str_detect(SIG_CD,"^36")) %>%
  ggplot(aes(x=long, y=lat, group=group))
