#loading libraries
library(odbc)
library(DBI)
library(RMySQL)
library(dplyr)
library(dbplyr)

con <- dbConnect(drv=MySQL(), user="sjdtg", password="sjdtg123", dbname="sjdtg", host="121.183.203.60", port=13306)

#listing tables
dbListTables(con)

#Create index and select a chunksize
index <- 0
chunkSize <- 200
memory.size()
memory.limit(10000000)

src_dbi(con)
surveys <- tbl(con, "CarInfo10_data")
colnames(surveys)<-c("trip_id", "D", "CD", "v", "RPM", "break","long","lat", "angle", "Vx","Vy", "Status_code", "Date_time","peak_time", "delta_v")



head(surveys, n = 10)

install.packages("ffply")
library(ffply)
#try<- dbReadTable(con,"CarInfo10_data", nrows=chunkSize, header=F, fill=TRUE, sep=",")
