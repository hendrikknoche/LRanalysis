# Import R libraries
# Add the ones you need below
library(RMySQL) # Import data table from LS database

source("config.R")

# Connect to server 
mydb = dbConnect(MySQL(), user=ODBCUID, password=pass, dbname="touchStudies", host="192.38.56.104")

# Send query 
rs<-dbSendQuery(mydb, "SELECT * FROM touchEvents")
my_data<- fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

#Display the data from the fil
View(my_data)
