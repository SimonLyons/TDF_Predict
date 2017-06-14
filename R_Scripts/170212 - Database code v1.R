# Testing database access skills
# install.packages("RMySQL")
require(RMySQL)

# Set working directory to location of database password file
# Run my own function to set the database password folder location
# The function asks the user whether they're using the Linux laptop
# or the Windows PC
dbpsswddir <- set_db_password()
setwd(dbpsswddir)
psswd <- read.csv("passwords_db.csv")

# Create connection to local MySQL server
# Local machine
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45', host="")
# Remote machine
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='192.168.1.1', port=3306) 

# Create ProCycling stats database
dbSendQuery(conn_local, "CREATE DATABASE ProCycling;")

# Create connection to Procycling database
# Local machine
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='localhost')
# Remote machine
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='192.168.1.1', port=3306)

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)



setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

# Read in calendar .csv files into dataframes and 
i <- 2016

for(i in 2005:2016){
  
  calendar_cn <- read.csv(paste("calendar_CN_", i, ".csv", sep = ""), header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
  dbWriteTable(conn_local, name = paste("calendar_CN_", i, sep = ""), calendar_cn, overwrite = TRUE)
  
}   # Close FOR loop

#################################################################
# Script to remove tables (requires 'manager' or 'admin' rights)
for(i in 2005:2016){
dbRemoveTable(conn_local, paste("calendar_cn_", i, sep = ""))
}
# End script to remove tables
#################################################################

# Some MySQL commands copied in early in my DB learning process
# ALTER TABLE goods ADD PRIMARY KEY(id)
# dbSendQuery(conn_local, "ALTER TABLE calendar_cn_2010 ADD PRIMARY KEY(`event.ID`);" )
# dbSendQuery(conn_local, "ALTER TABLE race_calendar_2006 CHANGE COLUMN race_id race_id INT PRIMARY KEY;")
# dbSendQuery(conn_local, 'ALTER TABLE race_calendar_2006 ALTER PRIMARY KEY(race_id);')
# alter table Persion add primary key (persionId,Pname,PMID)

# List db tables
dbListTables(conn_local, dbname='ProCycling')

# List 'fields' from particular table
dbListFields(conn_local, "race_calendar_2005")


# List all open db connections
dbListConnections(MySQL())

dbClearResult(dbListResults(conn_local)[[1]])

# Get info about db that's connected
dbGetInfo(conn_local)

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

dbDisconnect(dbListConnections(MySQL())[[1]])

###############################################
# Practice extracting from Procycling database

# Create connection to Procycling database
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='localhost')

# Test out some queries
query <- dbSendQuery(conn_local, "SELECT * FROM race_calendar_2005;")
new_df <- dbFetch(query)

query <- dbSendQuery(conn_local, "DESCRIBE race_calendar_2005;")
new_df <- dbFetch(query)
View(new_df)

query <- dbSendQuery(conn_local, "SELECT * FROM calendar_cn_2010 limit 5000;")
new_df <- dbFetch(query, n=500)
new_df[480:489,"Web.link"]

summary(new_df)
new_df[, "Web.link"]

tables_query <- dbSendQuery(conn_local, "SHOW tables;")
tables <- dbFetch(tables_query, n=-1)
View(tables)


query <- dbSendQuery(conn_local, "SHOW columns FROM race_weblinks_2016;")
new_df <- dbFetch(query, n=-1)
View(new_df)
new_df[480:489,"Web.link"]


query2 <- dbSendQuery(conn_local, "SELECT race_details AS `Race Name`, location, uci_code  
                      FROM race_calendar_2017;")
new_df2 <- dbFetch(query2, n=-1)
View(new_df2)

query3 <- dbSendQuery(conn_local, "SELECT * FROM race_weblinks_2017;")
new_df3 <- dbFetch(query3, n=-1)
View(new_df3)


master_time <- dbGetQuery(conn_local, "SELECT* FROM master_results_time;")
summary(master_time)

library(dplyr)
library(lubridate)
richie_porte <- master_time %>%
  filter(Rider == "Richie Porte") %>% 
  summarise(Total_Time = sum(dseconds(richie_porte$Duration)))
lubridate::