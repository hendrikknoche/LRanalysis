# This script is used to calculate the movementTime between touches.

library(RMySQL)
library(dplyr)

source("config.R")

# Connect to the database 
mydb = dbConnect(MySQL(), user = ODBCUID, password = pass, dbname = "touchStudies", host = "192.38.56.104")

# Save the data from touchEvents in the variable touchEvents_Data
rs<-dbSendQuery(mydb, "SELECT * FROM touchEvents")
touchEvents_Data <- data.frame()
touchEvents_Data <- fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])
#View(touchEvents_Data)

touchEvents_Data[c("MovementTime")] <- NA
touchEvents_Data

touchEvents_Data$Loph<-c(NA,df[1:nrow(touchEvents_Data)-1,]$LiftTime)







nrow(touchEvents_Data)
touchEvents_Data[nrow(touchEvents_Data) + 1,]
nrow(touchEvents_Data)
n <- 2

for(n in touchEvents_Data$ID){
  touchEvents_Data$MovementTime <- touchEvents_Data$TouchTime - touchEvents_Data$LiftTime
  n <- n + 1
  if(n > nrow(touchEvents_Data)){
    break()
  }
}

touchEvents_Data
