

# This script is aimed at building a dataset for a selected rider
# (in this case Simon Gerrans) and then displaying some graphics
# showing the change in their performance over their career

######################################################################
# Load required libraries
require(RMySQL)
require(lubridate)
require(dplyr)

# Set working directory
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")

######################################################################
# Define input variables
rider <- "Simon Gerrans"

######################################################################
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
# Build a MySQL query that filters on the rider (Simon Gerrans) and
# returns their race results, including the race name (race_details).
# This will come from the race_calendar_master & master_results_time tables

sql_search_criteria <- 

rider_data_query <- dbGetQuery(conn_local, sql_search_criteria)



