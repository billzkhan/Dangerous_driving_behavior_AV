#loading trip data
memory.limit(size=NA)

trip_data <- read.table("D:/BILAL/CarInfo10_trip.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
trip_data<- trip_data[,c(1,5)]

df <- read.table("D:/BILAL/output.csv",sep = ",", header = TRUE, stringsAsFactors = FALSE)

#renaming the column names
#names(df)<- c("trip_id", "D", "CD", "v", "RPM", "break","long","lat", "angle", "Vx","Vy", "Status_code", "Date_time","peak_time", "delta_v")
names(trip_data)<- c("trip_id","car_type")

#vlookup in r to add cartype into original data
data <- (merge(df, trip_data, by = 'trip_id'))

#solution for time in data
#replace the error language
data$Date_time<- gsub("오전", "AM", data$Date_time)
data$Date_time<- gsub("오후", "PM", data$Date_time)
#split into date and time
library(splitstackshape)
data<- cSplit(data, 'Date_time', sep=" ", type.convert=FALSE)
data$Date<- paste(data$Date_time_1)
data$time<- paste(data$Date_time_3,data$Date_time_2)

#removing irrelevant colmns
data<- subset(data, select=-c(16,17,18))

#converting time to 24 hr format
data$time <- format(strptime(data$time, "%I:%M:%S %p"), "%H:%M:%S")
class(data$time)
#converting to time clas
data$time<- as.POSIXct(data$time,format="%H:%M:%S")

#making categories of time
library(lubridate)
data$time_cat<- cut(hour(data$time), breaks = c(0,12,24), labels = c("AM","PM"))

#making a column of taxi and bus using coding scheme (21 code is for taxi and rest codes are for buses)
data$car_type_cat<- ifelse(data$car_type==21,"Taxi","Bus")


#finding functions
#install.packages("sos")
#library("sos")
#findFn("rollsum")

#install.packages("data.table")
library(data.table)
library(dplyr)

#making column of consecutive difference for angle by id
data<- data %>% group_by(trip_id) %>% mutate(Diff_angle = angle - lag(angle, default = first(angle)))
data$x1<- ifelse(data$Diff_angle>270,-1,0)
data$x2<- ifelse(data$Diff_angle<(-270),1,0)
data$x1<- data$x1*360
data$x2<- data$x2*360
data$Diff_angle<- data$Diff_angle+data$x1+data$x2

#making column for consecutive difference of speed by id
data<- data %>% group_by(trip_id) %>% mutate(Diff_v = v - lag(v, default = first(v)))

#data= data.table(data)

#................................CALCULATION FOR RISKY BEHAVIOR.............................
#install.packages("tidyverse")
#library(tidyverse)
#install.packages("RcppRoll")
library(RcppRoll) #library for roll_sum, library(zoo) for rollsum

#take rolling sum of every 5 by ids and then shifting it by 2 values to get 5 gap in the start
data<- data %>% group_by(trip_id) %>% mutate(cumm_angle5 = roll_sum(Diff_angle, 5, fill = NA))
data$cumm_angle5<- shift(data$cumm_angle5,2L, fill = NA)

#take rolling sum of every 5 by ids and then shifting it by 2 values to get 5 gap in the start
#data<- data %>% group_by(trip_id) %>% mutate(cumm_aord5 = roll_sum(Diff_v, 5, fill = NA))
#data$cumm_aord5<- shift(data$cumm_aord5,2L, fill = NA)

#take rolling sum of every 4 by ids and then shifting it by 2 values to get 4 gap in the start
data<- data %>% group_by(trip_id) %>% mutate(cumm_angle4 = roll_sum(Diff_angle, 4, fill = NA))
data$cumm_angle4<- shift(data$cumm_angle4,2L, fill = NA)

#take rolling sum of every 4 by ids and then shifting it by 2 values to get 4 gap in the start
#data<- data %>% group_by(trip_id) %>% mutate(cumm_aord4 = roll_sum(Diff_v, 4, fill = NA))
#data$cumm_aord4<- shift(data$cumm_aord4,2L, fill = NA)


#take rolling sum of every 8 by ids and then shifting it by 2 values to get 4 gap in the start
data<- data %>% group_by(trip_id) %>% mutate(cumm_angle8 = roll_sum(Diff_angle, 8, fill = NA))
data$cumm_angle8<- shift(data$cumm_angle8,4L, fill = NA)

#take rolling sum of every 8 by ids and then shifting it by 2 values to get 4 gap in the start
#data<- data %>% group_by(trip_id) %>% mutate(cumm_aord8 = roll_sum(Diff_v, 8, fill = NA))
#data$cumm_aord8<- shift(data$cumm_aord8,4L, fill = NA)

#take rolling sum of every 10 by ids and then shifting it by 2 values to get 4 gap in the start
data<- data %>% group_by(trip_id) %>% mutate(cumm_angle10 = roll_sum(Diff_angle, 10, fill = NA))
data$cumm_angle10<- shift(data$cumm_angle10,5L, fill = NA)

#take rolling sum of every 10 by ids and then shifting it by 2 values to get 4 gap in the start
#data<- data %>% group_by(trip_id) %>% mutate(cumm_aord10 = roll_sum(Diff_v, 10, fill = NA))
#data$cumm_aord10<- shift(data$cumm_aord10,5L, fill = NA)

#deleting the 10 sec one, not needed
data<- subset(data, select=-c(18,25))


#take rolling sum of every 3 by ids and then shifting it by 2 values to get 4 gap in the start
data<- data %>% group_by(trip_id) %>% mutate(cumm_angle3 = roll_sum(Diff_angle, 3, fill = NA))
data$cumm_angle3<- shift(data$cumm_angle3,1L, fill = NA)

#take rolling sum of every 3 by ids and then shifting it by 2 values to get 4 gap in the start
#data<- data %>% group_by(trip_id) %>% mutate(cumm_aord3 = roll_sum(Diff_v, 3, fill = NA))
#data$cumm_aord3<- shift(data$cumm_aord3,3L, fill = NA)

#take rolling sum of every 6 by ids and then shifting it by 2 values to get 4 gap in the start
data<- data %>% group_by(trip_id) %>% mutate(cumm_angle6 = roll_sum(Diff_angle, 6, fill = NA))
data$cumm_angle6<- shift(data$cumm_angle6,3L, fill = NA)

#take rolling sum of every 10 by ids and then shifting it by 2 values to get 4 gap in the start
#data<- data %>% group_by(trip_id) %>% mutate(cumm_aord6 = roll_sum(Diff_v, 6, fill = NA))
#data$cumm_aord6<- shift(data$cumm_aord6,3L, fill = NA)

#................................CALCULATION TO FIND ALL CONDITIONS.........................sep for taxi and bus

#now we have to apply two conditions to get the rapid lane change
#conditions are 1. changing lane more than 8/sec delta angle and speed >30km/hr, 2. cum delta angle <2/sec and cum aord>3km/hr
#for true conditions we will make a column denoting yes by 1 and no by 0


#rapid acceleration...for taxi and bus...............

for (i in nrow(data)) {
  if(data$car_type_cat[i]=="Taxi") {
    
    data<- data %>% 
      mutate(Ra = ifelse(
        (v>=6) & (Diff_v>=8),"Yes", "No"))
  }
  else {
    data<- data %>% 
      mutate(Ra = ifelse(
        (v>=6)& (Diff_v>=6), "Yes", "No")
      )
    
  }
  
}

#rapid deceleration............................

for (i in nrow(data)) {
  if(data$car_type_cat[i]=="Taxi"){
    
    data<- data %>% 
      mutate(Rd = ifelse(
        
        (Diff_v<=-14) & (v>=6),"Yes", "No")
      )
  }
  else {
    
    data<- data %>% 
      mutate(Rd = ifelse(
        
        (Diff_v<=-9) &(v>=6),"Yes", "No")
      )
  }
}


#rapid stop..................................

for (i in nrow(data)) {
  if(data$car_type_cat[i]=="Taxi"){
    
    data<- data %>% 
      mutate(Rstop = ifelse(
        
        (Diff_v<=-14) & (v<=5),"Yes", "No")
      )
  }
  else{
    
    data<- data %>% 
      mutate(Rstop = ifelse(
        
        (Diff_v<=-9) & (v<=5), "Yes", "No")
      )
  }  
}


#RLcahnge...........................................
for(i in nrow(data))
{
  if(data$car_type_cat[i]=="Taxi")
  {
    data<- data %>% 
      mutate(RLchange = ifelse(
        
        (Diff_angle<=-10 | Diff_angle>=10) &
          (v>=30)                            &
          (cumm_angle5<=2 & cumm_angle5>=-2) &
          (between(Diff_v,-2,2)),
        "Yes", "No")
      )        
    
  }
  else
  {
    data<- data %>% 
      mutate(RLchange = ifelse(
        
        (Diff_angle<=-8 | Diff_angle>=8) &
          (v>=30)                          &
          (cumm_angle5<=2 & cumm_angle5>=-2) &
          (between(Diff_v,-2,2)),
        "Yes", "No")
      )        
    
  }
}


#Rovertaking....................................

# Applying for rapid overtaking

for (i in nrow(data)){
  if(data$car_type_cat[i]=="Taxi"){
    
    data<- data %>% 
      mutate(Rovertaking = ifelse(
        
        (Diff_angle<=-10 | Diff_angle>=10) &
          (v>=30)                          &
          (cumm_angle5<=2 & cumm_angle5>=-2) &
          (Diff_v>=3),
        "Yes", "No")
      )
    
  }
  else {
    data<- data %>% 
      mutate(Rovertaking = ifelse(
        
        (Diff_angle<=-8 | Diff_angle>=8) &
          (v>=30)                          &
          (cumm_angle5<=2 & cumm_angle5>=-2) &
          (Diff_v>=3),
        "Yes", "No")
      )
    
  }
  
}


#extra....3,6 sec cummulation for taxi and 4,8 for bus
#rapid turn.....................................................

for(i in nrow(data))
{
  if(data$car_type_cat[i]=="Taxi")
  {
    data<- data %>% 
      mutate(Rturn = ifelse(
        (v>=30) & (between(cumm_angle3,60,120)|between(cumm_angle3,-120,-60)),
        "Yes", "No")
      )  
    
  }
  else
  {
    data<- data %>% 
      mutate(Rturn = ifelse(
        (v>=25) & (between(cumm_angle4,60,120)|between(cumm_angle4,-120,-60)),
        "Yes", "No")
      )   
    
  }
}


#rapid U turn

for(i in nrow(data))
{
  if(data$car_type_cat[i]=="Taxi")
  {
    data<- data %>% 
      mutate(Ruturn = ifelse(
        (between(cumm_angle6,160,180)|between(cumm_angle6,-180,-160)) & (v>=25),
        "Yes", "No")
      )
    
  }
  else
  {
    data<- data %>% 
      mutate(Ruturn = ifelse(
        (between(cumm_angle8,160,180)|between(cumm_angle8,-180,-160)) & (v>=20),
        "Yes", "No")
      )
  }
}

#exporting in the form of csv file
write.csv(data,"D:/BILAL/output_week.csv", row.names = TRUE)



#memory issues
memory.size()
memory.limit()
