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


#          ### WHERE filter based on rider###
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


#          ### WHERE filter based on date ###
# Build a WHERE criteria list of a date range for each year prior to the start of the TdF

# Searc for Tdf in race_calendar_master

# Basic string matching
TdF_dates <- dbGetQuery(conn_local, "SELECT * FROM race_calendar_master
                                                  WHERE start_date LIKE '%Tour De France%';")

# More advanced fuzzy string matching using SOUNDEX()
TdF_dates <- dbGetQuery(conn_local, "SELECT * FROM race_calendar_master
                                                  WHERE SOUNDEX(start_date) = SOUNDEX('Tour De France');")

# The next bit regarding picking the earliest date for each year might
# be achieved by using the GROUP BY function and the MIN function
# https://stackoverflow.com/questions/5736820/sql-how-to-select-earliest-row
TdF_dates <- dbGetQuery(conn_local, "SELECT race details, MIN(start_date) 
                                                  FROM race_calendar_master
                                                  WHERE SOUNDEX(start_date) = SOUNDEX('Tour De France')
                                                  GROUP BY YEAR(start_date);")

View(TdF_dates)



# Yet to be populated with the correct dates.
TdF_start_dates <- c("2016-06-03", "2015-06-03", "2014-06-03", "2013-06-03", "2012-06-03", "2011-06-03", "2010-06-03")

# Build a dataframe with the date ranges, and a field with the text of the range
pre_TdF_date_range <- as.data.frame(matrix(data = NA, ncol = 3, nrow = length(TdF_start_dates)))
colnames(pre_TdF_date_range) <- c("TdF_start", "year_start", "WHERE_date")
pre_TdF_date_range_list <- NA
for (d in 1:length(TdF_start_dates)){
  pre_TdF_date_range[d, 1] <- TdF_start_dates[d]
  pre_TdF_date_range[d, 2] <- paste0(year(TdF_start_dates[d]), "-01-01")
  pre_TdF_date_range[d, 3] <- paste0("(stage_date >= '", pre_TdF_date_range[d, 2], "' AND stage_date < '", pre_TdF_date_range[d, 1], "')")
  pre_TdF_date_range_list <- paste0(pre_TdF_date_range_list, " OR ", pre_TdF_date_range[d, 3])
}
# Get rid of the 'NA OR' at the start of the MySQL date range list
pre_TdF_date_range_list <- gsub("NA OR ", "", pre_TdF_date_range_list)

# WHERE_date <- "stage_date >= '2016-01-01' AND stage_date < '2016-06-03'"

# Paste together the criteria for the MySQL query
criteria <- paste0("SELECT * FROM master_results_time WHERE ", pre_TdF_date_range_list, ";")

# Perform the MySQL query, 
date_table <- dbGetQuery(conn_local, criteria)
View(date_table)
