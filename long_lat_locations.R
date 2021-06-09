# ggmap 2.7을 설치합니다. (아직 CRAN에 등록되어 있지 않습니다.)
devtools::install_github('dkahle/ggmap')
# 필요 패키지를 불러옵니다. 
library(ggmap)

register_google(key = 'AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU')
get_map(location = '세종',
        zoom = 14,
        maptype = 'roadmap',
        source = 'google') %>% 
  ggmap()

#install.packages("googleway")
library(googleway)
x<-google_reverse_geocode(location = c(36.49663,127.2573), result_type = c('administrative_area_level_5'), location_type = "rooftop",
                       key = "AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU", language = "ko")
x
y<- x$results$address_components
z<- x$results$formatted_address
z

xx<-google_snapToRoads(data, lat = 'lat', lon = 'long', key = "AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU")
xx
