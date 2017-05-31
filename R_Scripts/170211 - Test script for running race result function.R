# Master script for calling functions to extract race results data
# from the Cycling News

#################################################
# Create files for CN race calendars
# This essentially only needs to be done once.
# 
# Define range of years. Can be modified below.
start_year <- 2008
end_year <- 2017

# Run 'initialCNCalendar' function
# This function now includes the script to clean
# and remove problem characters (including 'removeDiscritics')
initialCNCalendar(start_year, end_year)

tad2 <- initialCNCalendar(start_year, end_year)
View(tad2 %>% select(race_details, location))

require(dplyr)
require(stringr)
prob <- tad %>% select(race_details) %>% filter(str_detect(race_details, "The Noreaster"))



#################################################

# Function to go and extract all of the race results tables for an entire calendar year
require(RMySQL)


# Define the year
input_year <- 2011

# Begin function
GetAllRacesInAYear <- function(input_year){

# Read in CN calendar csv file
# calendar_CN_year <- read.csv(paste("calendar_CN_", input_year, "_final.csv", sep = ""), header = TRUE, sep = ","))

# Extract (into dataframe) the following columns Web.link	Start.date	End.date	event.name	event.ID

# Go to each Web.link and extract the race web links for each event.ID. Create race.ID
# So we'll combine the event.ID with a sequential number to create the race.ID
# Create a dataframe for each event with columns for the race weblink, date, start_location, finish_location
# 02_function_obtain_event_race_results_weblinks.R   has the function 'write_race_results_tables'
  
  
  for(input_year in 2016:2017){
  Race_Weblink_Year <- GetRaceWebLinks(input_year)  
  }
  View(Race_Weblink_Year)
  
  class(Race_Weblink_Year$stage_date[23])
  new_date <- as_date(Race_Weblink_Year$stage_date[23])
  class(new_date)

  
# Open each race weblink and extract tables
# 03_function_race_results_table.R

  # Set working directory to user passwords location
  setwd("C:/b_Data_Analysis/Database")
  # Read database password file
  psswd <- read.csv("passwords_db.csv", header = TRUE)
  
  input_year <- 2016   # Manual input of selected year
  
  # Create connection to database 
  conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , 
                          password = as.character(psswd[psswd$type == "Manager", "password"]),  
                          dbname='ProCycling', host='localhost')   
  query <- dbSendQuery(conn_local, "SELECT * FROM race_weblinks_Master
                       LIMIT 10 OFFSET 2200;")
  Race_Weblink_Year <- dbFetch(query, n=-1)
  View(Race_Weblink_Year)
  
  ########################################################
  ########################################################
  ########################################################
  # Testing with new race weblink format
  Race_Weblink_Year <- dbGetQuery(conn_local, "SELECT * FROM race_weblinks_2015 LIMIT 10 OFFSET 200;")
  

  test_test <- dbGetQuery(conn_local, "SELECT * FROM test_test_master_results_time;")
  View(test_test)
  
  
  glimpse(points_tables)
  as.integer(points_tables$Result)
  points_tables$Rider
  glimpse(time_tables)
  

  ########################################################
  ########################################################
  ########################################################

    
  # Use Text Progress Bar
  total <- nrow(Race_Weblink_Year)
  # create text progress bar
  prg <- txtProgressBar(min = 0, max = total, style = 3)
  
  for (r in 1: nrow(Race_Weblink_Year)){
    Sys.sleep(0.1)
    # Setup text-based progress bar
    setTxtProgressBar(prg, r)
    
    write_race_results_tables(Race_Weblink_Year[r, "stage_url"], Race_Weblink_Year[r, "stage_id"], Race_Weblink_Year[r, "date"])
    
    # Sleep function to randomise web queries
    sleep <- abs(rnorm(1)) + runif(1, 0, .25)
    message("I have done ", r, " of ", nrow(Race_Weblink_Year),
            " - gonna sleep ", round(sleep, 2),
            " seconds.")
    Sys.sleep(sleep)
    
    }   # End FOR loop to retrieve race results tables using function "write_race_results_tables"
        # 'write_race_results_tables' function comes from 03_function_race_results_tables.R
  close(prg)   # Text Progress Bar script 
  
  
# Create a dataframe for each race result table. Name it for the race.ID

}   # End overall function 'GetAllRacesInAYear'





#################################################
# Script for extracting data for all riders
# This essentially only needs to be done once.
# and then updated annually.
# 
# This function writes a table for each year
# to the database in name format 'riderlist_20YY'
# 
# Define range of years. Can be modified below.
start_year <- 2006
end_year <- 2017
# 
# Run 'getRiderList' function
getRiderList(start_year, end_year)
# 
#################################################

#################################################
# Script for extracting data for all riders and
# then creating a single master list of riders
# with one entry for each rider
# 
# Define range of years. Can be modified below.
start_year <- 2005
end_year <- 2017
# 
# Run 'riderMasterList' function
riderMasterList(start_year, end_year)
# 
#################################################


##  Check rider_list_master in ProCycling Database
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='localhost')
query1 <- dbSendQuery(conn_local, "SELECT * FROM rider_list_master;")
new_df1 <- dbFetch(query1, n=-1)   # Note the 'n=-1' is required to return all rows, otherwise the database only returns a max of 500!!
head(new_df1)
nrow(new_df1)
names(new_df1)
nrow(new_df[new_df$nationality == "United States", ])
unique(new_df$team_name)
new_df[new_df$team_name == "Cannondale-Drapac", ]


##  Check race_calendar tables in ProCycling Database
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='localhost')
query1 <- dbSendQuery(conn_local, "SELECT * FROM race_calendar_2017;")
new_df1 <- dbFetch(query1, n=-1)   # Note the 'n=-1' is required to return all rows, otherwise the database only returns a max of 500!!
head(new_df1)
nrow(new_df1)
names(new_df1)

dbDisconnect(dbListConnections(MySQL())[[1]])

