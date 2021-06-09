#finding functions
#install.packages("sos")
library("sos")
#findFn(":=")

#install.packages("data.table")
library(data.table)

#loading the csv or sql server file
df <- read.csv("D:/BILAL/BILAL try.csv")
str(df)

#changing names of column headers......example
names(df)<- c('x','y','z','a','b','c')


#making column of consecutive difference for angle by id
df<- df %>% group_by(trip_id) %>% mutate(Diff_angle = angle - lag(angle, default = first(angle)))

#making column for consecutive difference of speed by id
df<- df %>% group_by(trip_id) %>% mutate(Diff_v = v - lag(v, default = first(v)))

df= data.table(df)

#take rolling sum of every 5 by ids and then shifting it by 2 values to get 5 gap in the start
df<- df %>% group_by(trip_id) %>% mutate(cumm_angle5 = rollsum(Diff_angle, 5, fill = NA))
df$cumm_angle5<- shift(df$cumm_angle5,2L, fill = NA)

#take rolling sum of every 5 by ids and then shifting it by 2 values to get 5 gap in the start
df<- df %>% group_by(trip_id) %>% mutate(cumm_aord5 = rollsum(Diff_v, 5, fill = NA))
df$cumm_aord5<- shift(df$cumm_aord5,2L, fill = NA)

#take rolling sum of every 4 by ids and then shifting it by 2 values to get 4 gap in the start
df<- df %>% group_by(trip_id) %>% mutate(cumm_angle4 = rollsum(Diff_angle, 4, fill = NA))
df$cumm_angle4<- shift(df$cumm_angle4,2L, fill = NA)

#take rolling sum of every 4 by ids and then shifting it by 2 values to get 4 gap in the start
df<- df %>% group_by(trip_id) %>% mutate(cumm_aord4 = rollsum(Diff_v, 4, fill = NA))
df$cumm_aord4<- shift(df$cumm_aord4,2L, fill = NA)


#take rolling sum of every 9 by ids and then shifting it by 2 values to get 4 gap in the start
df<- df %>% group_by(trip_id) %>% mutate(cumm_angle9 = rollsum(Diff_angle, 9, fill = NA))
df$cumm_angle9<- shift(df$cumm_angle9,4L, fill = NA)

#take rolling sum of every 9 by ids and then shifting it by 2 values to get 4 gap in the start
df<- df %>% group_by(trip_id) %>% mutate(cumm_aord9 = rollsum(Diff_v, 9, fill = NA))
df$cumm_aord9<- shift(df$cumm_aord9,4L, fill = NA)

#now we have to apply two conditions to get the rapid lane change
#conditions are 1. changing lane more than 8/sec delta angle and speed >30km/hr, 2. cum delta angle <2/sec and cum aord>3km/hr
#for true conditions we will make a column denoting yes by 1 and no by 0

df<- df %>% 
        mutate(RLchange = ifelse(
          
            (Diff_angle<=-8 | Diff_angle>=8) &
            (v>=30)                          &
            (cumm_angle5<=2 | cumm_angle5<=-2) &
            (cumm_aord5>=2 | cumm_aord5>=-2),
            "Yes", "No")
        )

# Applying for rapid overtaking

df<- df %>% 
  mutate(Rovertaking = ifelse(
    
    (Diff_angle<=-8 | Diff_angle>=8) &
      (v>=30)                          &
      (cumm_angle5<=2 | cumm_angle5<=-2) &
      (cumm_aord5>=3 | cumm_aord5>=-3),
    "Yes", "No")
  )


#rapid acceleration

df<- df %>% 
  mutate(Ra = ifelse(
    
    (Diff_v>=6) &
    (v>=6),
    "Yes", "No")
  )

#rapid start

df<- df %>% 
  mutate(Rs = ifelse(
    
    (Diff_v>=8) &
    (v<=5),
    "Yes", "No")
  )

#rapid deceleration


df<- df %>% 
  mutate(Rd = ifelse(
    
    (Diff_v<=-9) &
      (v>=6),
    "Yes", "No")
  )

#rapid stop

df<- df %>% 
  mutate(Rstop = ifelse(
    
    (Diff_v<=-9) &
    (v<=5),
    "Yes", "No")
  )

#extra....but it is done for 5 sec cummulation
#rapid turn

df<- df %>% 
  mutate(Rturn = ifelse(
    (v>=25)&  
    (cumm_angle4>=60 & cumm_aord4<=120)| (cumm_angle4<=-60 & cumm_aord4>=-120),
    "Yes", "No")
  )

#rapid U turn

df<- df %>% 
  mutate(Ruturn = ifelse(
    (v>=25)&
    (cumm_angle9>=160 & cumm_angle9<=180) | (cumm_angle9<=-160 & cumm_angle9>=-180),
    "Yes", "No")
  )


#exporting in the form of csv file
write.csv(df,"D:/BILAL/output.csv", row.names = TRUE)


