# This script aims to build predictor variables, for each 
# rider completing the 2016 TDF, with the total number of 
# kilometres and hours they've ridden each year, prior to
# the start of the Tour de France.


# Load required libraries
require(RMySQL)
require(lubridate)
require(dplyr)

# Read prior file with details of riders finishing the 2016 TdF
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
View(results_df)

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



# Build a WHERE criteria list of all the riders who completed the 2016 TdF
WHERE_rider <- paste0("Rider = '", results_df$Rider[1], "'")
for (r in 2:length(results_df$Rider)){
  WHERE_rider <- paste0(WHERE_rider, " OR Rider = '", results_df$Rider[r], "'")
}

# Need to build in a WHERE criteria for only the dates in each year prior to the start of the TDF
# At the moment I'll continue my variable building with just the downselect of riders

# Paste together the criteria for the MySQL query
criteria <- paste0("SELECT * FROM master_results_time WHERE ", WHERE_rider, ";")

# Perform the MySQL query, 
rider_table <- dbGetQuery(conn_local, criteria)
View(rider_table)
rider_table$stage_date <- as.Date(rider_table$stage_date)
glimpse(rider_table)

# Aggregate rider result data to get a total duration for each rider, for each year
total_duration <- rider_table %>% 
  group_by(year(stage_date), Rider) %>% 
  summarise(sum(duration, na.rm = TRUE))   # It's important to remove NAs

# Just take a look at the results for Chris Froome
CF_td <- total_duration %>% filter(Rider == 'Christopher Froome') %>% mutate("Duration_s" = )

