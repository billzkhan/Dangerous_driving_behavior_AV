Sys.Date()
install.packages("lubridate")
lubridate::today()

Sys.time()
lubridate::now()

lubridate::now(tzone = "UTC")

copy <- read.csv("D:/BILAL/data_copy.csv")
names(copy)<- c("trip_id", "D", "CD", "v", "RPM", "break","long","lat", "A", "Vx","Vy", "Status_code", "Date_time","peak_time", "delta_v")
str(copy)

copy$Date_time<- gsub("오전", "AM", copy$Date_time)
copy$Date_time<- gsub("오후", "PM", copy$Date_time)

#install.packages("splitstackshape")
library(splitstackshape)
copy<- cSplit(copy, 'Date_time', sep=" ", type.convert=FALSE)
copy$Date_time<- paste(copy$Date_time_1,copy$Date_time_3,copy$Date_time_2)
copy$time<- paste(copy$Date_time_3,copy$Date_time_2)

#converting time to 24 hr format
copy$time <- format(strptime(copy$time, "%I:%M:%S %p"), "%H:%M:%S")

copy$time<- as.POSIXct(copy$time,format="%H:%M:%S")
copy$time_cat<- cut(hour(copy$time), breaks = c(0,6,12,24))

class(x)
#converting time to time class
#install.packages("chron")
library(chron)
copy$time <- chron(times=copy$time)
class(time)

class(copy$Date_time_2)
as.factor(copy$Date_time_2)
copy$Date_time_2[copy$Date_time_2 == "오전"] <- "AM"
copy$Date_time_2[copy$Date_time_2 == "오후"] <- "PM"


