
# Testing database access skills

require(RMySQL)

# Create connection to local MySQL server
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45', host='localhost')

# Create ProCycling stats database
dbSendQuery(conn_local, "CREATE DATABASE ProCycling;")

# Create connection to Procycling database
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='localhost')

setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

# Read in calendar .csv files into dataframes and 
i <- 2016

for(i in 2005:2016){
  
  calendar_cn <- read.csv(paste("calendar_CN_", i, ".csv", sep = ""), header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
  dbWriteTable(conn_local, name = paste("calendar_CN_", i, sep = ""), calendar_cn)
  
}   # Close FOR loop

#################################################################
# Script to remove tables (requires 'manager' or 'admin' rights)
for(i in 2005:2016){
dbRemoveTable(conn_local, paste("calendar_cn_", i, sep = ""))
}
# End script to remove tables
#################################################################

dbSendQuery(conn_local, "ALTER TABLE calendar_cn_2010 ADD PRIMARY KEY(`event.ID`);" )

ALTER TABLE goods ADD PRIMARY KEY(id)

# List db tables
dbListTables(conn_local, dbname='ProCycling')


dbListFields(conn_local, "calendar_cn_2010")

# List all open db connectoins
dbListConnections(MySQL())

dbClearResult(dbListResults(conn_local)[[1]])

query <- dbSendQuery(conn_local, "SELECT * FROM calendar_cn_2010 limit 5000;")
new_df <- dbFetch(query, n=500)
new_df[480:489,"Web.link"]

summary(new_df)
new_df[, "Web.link"]

# Get info about db that's connected
dbGetInfo(conn_local)

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)



dbDisconnect(dbListConnections(MySQL())[[1]])