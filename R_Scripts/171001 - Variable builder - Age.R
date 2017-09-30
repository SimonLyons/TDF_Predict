# This script aims to build predictor variables, for each 
# rider completing the 2016 TDF, with their AGE.

######################################################################
# SETUP

# Load required libraries
require(RMySQL)
require(lubridate)
require(dplyr)
require(ggplot2)
require(tidyr)
require(caret)


# Read prior file with details of riders finishing the 2016 TdF
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
# View(results_df)

# Setup database connection
dbpsswddir <- set_db_password()
setwd(dbpsswddir)
psswd <- read.csv("passwords_db.csv")

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)
# Open connection to ProCyling database
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='192.168.1.7', port=3306, timeout=3600)

######################################################################
# MySQL QUERIES
# 
# I'm going to build a query pulling rider details from the rider table
# in the ProCycling database.
# 
#          ### WHERE filter based on rider###
# Build a WHERE criteria list of all the riders who completed the 2016 TdF
WHERE_rider <- paste0("Rider = '", results_df$Rider[1], "'")
for (r in 2:length(results_df$Rider)){
  WHERE_rider <- paste0(WHERE_rider, " OR Rider = '", results_df$Rider[r], "'")
}

# Paste together the criteria for the MySQL query
########## NEED TO ADJUST THE MySQL TABLE AT WHICH THE QUERY IS POINTING ##########
criteria <- paste0("SELECT * FROM master_results_time WHERE ", WHERE_rider, ";")

# Perform the MySQL query, 
rider_table <- dbGetQuery(conn_local, criteria)
View(rider_table)
# Convert DOB column to 'Date' class
########## NEED TO CHANGE 'stage.date' TO THE DOB COLUMN IN THE DATABASE TABLE
rider_table$stage_date <- as.Date(rider_table$stage_date)
glimpse(rider_table)

