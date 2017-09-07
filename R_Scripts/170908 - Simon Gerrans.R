
# Load required packages
require(RMySQL)
require(lubridate)
require(dplyr)

setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")

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

# Query master_results_time table for Simon Gerrans
# Pull the master_results_time table
master_results_time <- dbGetQuery(conn_local, "SELECT * FROM master_results_time WHERE Rider = 'Simon Gerrans';")
View(master_results_time)
str(master_results_time)
master_results_time$stage_date <- as.Date(master_results_time$stage_date)
mutate(master_results_time, year(stage_date))

write.csv(master_results_time, "master_results_time.csv", row.names = FALSE)

# Determine how many minutes Simon has ridden in each year
simon_minutes_year <- master_results_time %>% 
  filter(!is.na(duration)) %>% 
  group_by("Year" = year(stage_date)) %>% 
  summarise("Hours_Riding" = sum(duration)/3600)
View(simon_minutes_year)

write.csv(simon_minutes_year, "simon_minutes_year.csv", row.names = FALSE)
