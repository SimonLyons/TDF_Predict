# This script aims to build predictor variables, for each 
# rider completing the 2016 TDF, with the total number of 
# kilometres and hours they've ridden each year, prior to
# the start of the Tour de France.

######################################################################
# SETUP

# Load required libraries
require(RMySQL)
require(lubridate)
require(dplyr)
require(ggplot2)
require(tidyr)


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

######################################################################
# MySQL QUERIES
# 
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

# Search for Tdf in race_calendar_master

# Basic string matching - returns a variety of races with 'Tour de France in the race_details column
TdF_dates <- dbGetQuery(conn_local, "SELECT * FROM race_calendar_master
                                                  WHERE race_details LIKE '%Tour De France%';")

# More advanced fuzzy string matching using SOUNDEX()
# On first testing appears to return a far more select group of results
# Also appears to match CAPS or NO CAPS. Bonus!
TdF_dates <- dbGetQuery(conn_local, "SELECT * FROM race_calendar_master
                                                  WHERE SOUNDEX(race_details) = SOUNDEX('%Tour De France%');")

# The next bit regarding picking the earliest date for each year might
# be achieved by using the GROUP BY function and the MIN function
# https://stackoverflow.com/questions/5736820/sql-how-to-select-earliest-row
TdF_dates <- dbGetQuery(conn_local, "SELECT race_details, MIN(start_date) 
                                                  FROM race_calendar_master
                                                  WHERE SOUNDEX(race_details) = SOUNDEX('Tour De France')
                                                  GROUP BY YEAR(start_date);")
View(TdF_dates)

# Build list of TdF start dates, limited to 2010 and above
TdF_start_dates <- TdF_dates %>% filter(year(`MIN(start_date)`) > 2009) %>% select(`MIN(start_date)`) %>% unlist()

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
criteria <- paste0("SELECT * FROM master_results_time WHERE (", pre_TdF_date_range_list, ") AND (", WHERE_rider ,");")

# Perform the MySQL SUPER query, 
date_table <- dbGetQuery(conn_local, criteria)
write.csv(date_table, "date_table.csv", row.names = FALSE)
View(date_table)

######################################################################
# MANIPULATE DATA FROM MySQL QUERY
# 
# Read in the combined data table
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_data")
date_table <- read.csv("date_table.csv", header = TRUE)

# Filter down the table to just rows containg 'Results' or 'results'
# We don't want the 'General Classification', 'Young Rider' or other classifications
just_results <- date_table %>% filter(grepl(('Results|results'), result_class))

# Let's summarise by summing up the total racing time (in hours) for
# each rider, in each year.
rider_duration <- just_results %>% 
  group_by(Rider, "Year" = year(stage_date)) %>% 
  summarise("Riding_Time_hrs" = sum(duration, na.rm = TRUE)/3600)
View(rider_duration)
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
write.csv(rider_duration, "rider_duration.csv", row.names = FALSE)

# Reformat the table so the years are spread out as columns
# This format is useful for me eye-balling the data, but ggplot
# requires the data in the long format created above: rider_duration
rider_duration_spread <- tidyr::spread(rider_duration, Year, Riding_Time_hrs)
# Quick comparison of annual pre-TdF riding duration between Adam Hansen and Chris Froome
rd_AH_SG <- rider_duration_spread %>% filter(Rider == "Adam Hansen" | Rider == "Christopher Froome")
View(rider_duration_spread)
write.csv(rider_duration_spread, "rider_duration_spread.csv", row.names = FALSE)

### Identify team mates for each of the top twenty riders

# List of top 20 riders
# Read prior file with details of riders finishing the 2016 TdF
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
View(results_df)
# Order combined_rider_data by 2016 finishing position
top_20 <- results_df %>% arrange(X2016_FP) %>% filter(row_number() %in% 1:20)
unique(top_20$Team)

# Build a list of Chris Froome's team mates
top_20[top_20$Rider == "Christopher Froome", 3]   # Identify Chris Froome's team
top_20 %>% filter(Rider == "Christopher Froome") %>% select(Team) %>% as.list()   # Identify Chris Froome's team

# Identify Chris Froome's team mates in 2016
cf_tm <- results_df[results_df$Team ==  top_20[top_20$Rider == "Christopher Froome", 3]  & results_df$Rider != "Christopher Froome", 1]
View(cf_tm)

# Determine how many of these team mates completed the TdF in 2015
results_df %>% filter(Rider %in% cf_tm & !is.na(X2015_FP)) %>% summarise(n())
results_df %>% filter(Rider %in% cf_tm & !is.na(X2014_FP)) %>% summarise(n())
results_df %>% filter(Rider %in% cf_tm & !is.na(X2013_FP)) %>% summarise(n())
results_df %>% filter(Rider %in% cf_tm)

# Do a mutate to create a new column with the number of team mates ridden in 2016
results_df_tm <- results_df %>% 
  mutate("tm_2015" = )

View(results_df %>% mutate('new_column' = 
                             results_df$Rider %in% results_df[results_df$Team == results_df[results_df$Rider == results_df$Rider, 3], 1] & !is.na(X2015_FP)))

results_df %>% filter(Team == (filter(results_df, Rider == 
                                        "Christopher Froome") %>% select(Team) %>% unlist()) & Rider !=
                                        "Christopher Froome") %>% select(Rider) %>% unlist

# This script works, returning the list of Chris Froome's team mates, minus Chris Froome
cf_tm <- results_df %>% filter(Team == (filter(results_df, Rider == 
                                                    "Christopher Froome") %>% select(Team) %>% unlist()) & Rider !=
                                    "Christopher Froome") %>% select(Rider) %>% unlist





# The below is successfully filling out a column in the dataframe with a list of 
# Chris Froome's team mates. Unfortunately I'm having trouble taking this to the 
# next level by having a row wise list of the team mates for each rider.
new_df <-  results_df %>% rowwise() %>% mutate( 'rider_list' = (filter(results_df, Team == (filter(results_df, Rider == 
                                                      "Christopher Froome") %>% select(Team) %>% unlist()) & Rider !=
                                      "Christopher Froome") %>% select(Rider) %>% list()))
View(new_df)
new_df$rider_list[1]

# Returns a count of the riders in each team.
results_df %>% count(Team)




######################################################################
# DATA VISUALISATION
# 
# This is a straight plot of all of the rider annual riding hours totals for all of the riders
# The result is more crazy art than insightful data visualisation!
ggplot(data = rider_duration, aes(x = Year, y = Riding_Time_hrs)) + geom_line(aes(colour = Rider), size = 1.2) + 
  labs(title ="Total Riding Time Per Year (hrs)", x = "Year", y = "Hours") + theme(legend.position = "none")


# Build a dataframe with the above total riding hrs per year in addition
# to each riders TdF finishing position from the 
# results_df/"anal_df_2016_C&T.csv" dataframe
combined_rider_data <- merge(results_df, rider_duration_spread, by.x = "Rider", by.y = "Rider", all = TRUE)
View(combined_rider_data)

# Need to munge the combined_rider_data dataframe into a long format
# I'll first pull out the data for 2016 finishind position and total hrs

# Set working directory to location on work laptop
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_data/")
rider_duration <- read.csv("rider_duration.csv", header = TRUE)
rider_duration_spread <- read.csv("rider_duration_spread.csv", header = TRUE)
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
combined_rider_data   # As above with the merge function

# Order combined_rider_data by 2016 finishing position
top_20 <- combined_rider_data %>% arrange(X2016_FP) %>% filter(row_number() %in% 1:20)

# Create variables for insertion into plot function based on selected year
year_select <- 2014
var_FP <- paste0("X", year_select, "_FP")   # The finishing position column for the selected year
var_HR <- paste0("X", year_select)   # The number of hours ridden for the selected year

# Create the plot of finishing position versus the hours ridden (for selected year)
ggplot(top_20, aes_string(var_FP, var_HR, colour = "Team")) + 
  geom_point() + theme(legend.position="bottom") + geom_text(aes(label=Rider),hjust=0, vjust=0)

