

# This script is aimed at building a dataset for a selected rider
# (in this case Simon Gerrans) and then displaying some graphics
# showing the change in their performance over their career

######################################################################
# Load required libraries
require(RMySQL)
require(lubridate)
require(dplyr)
require(tidyr)

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

sql_search_criteria <- paste0("SELECT *
                              FROM master_results_time t
                              JOIN race_weblinks_master w ON t.stage_id = w.stage_id
                              JOIN race_calendar_master c ON c.race_id = w.race_id
                              WHERE t.rider = 'Simon Gerrans';")

sg_data_query <- dbGetQuery(conn_local, sql_search_criteria)
View(sg_data_query)
getwd()
write.csv(sg_data_query, "sg_data_query.csv", row.names = FALSE)

######################################################################
# Read Simon Gerrans data in
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
sg_data <- read.csv("sg_data_query.csv", header = TRUE)
View(sg_data)

# Build a list of races by year
races_by_year <- sg_data %>%
  select(start_date, race_details) %>% 
  group_by("Year" = year(start_date), race_details) %>% 
  distinct()
View(race_by_year)

# Determine the number of unique races per year
races_per_year <- races_by_year %>%
  group_by(Year) %>% 
  summarise("races_per_year" = n())
View(races_per_year)

# Attempting to build a table showing in which year Simon raced each unique event
new_table <- spread(races_by_year, key = Year, value = race_details) %>% select(2:9)
# At present the above table has a single line for each year. I want to have a single
# line for each race showing occurences in each year (a column exists for each year)


gather

unique_races <- unique(race_by_year$race_details) %>% sort()
years <- unique(races_by_year$Year)
unique_races %in% 
