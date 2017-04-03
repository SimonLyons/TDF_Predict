# Master script for calling functions to extract race results data
# from the Cycling News

#################################################
# Create files for CN race calendars
# This essentially only needs to be done once.
# 
# Define range of years. Can be modified below.
start_year <- 2011
end_year <- 2011

# Run 'initialCNCalendar' function
# This function now includes the script to clean
# and remove problem characters (including 'removeDiscritics')
initialCNCalendar(start_year, end_year)

tad <- initialCNCalendar(start_year, end_year)


require(dplyr)
require(stringr)
prob <- tad %>% select(race_details) %>% filter(str_detect(race_details, "The Noreaster"))
grep("‘", prob)
gsub("[^[:alnum:]///' ]", "", prob)


#################################################

# Function to go and extract all of the race results tables for an entire calendar year
# Define the year
input_year <- 2009

# Begin function
GetAllRacesInAYear <- function(input_year){

# Read in CN calendar csv file
# calendar_CN_year <- read.csv(paste("calendar_CN_", input_year, "_final.csv", sep = ""), header = TRUE, sep = ","))

# Extract (into dataframe) the following columns Web.link	Start.date	End.date	event.name	event.ID

# Go to each Web.link and extract the race web links for each event.ID. Create race.ID
# So we'll combine the event.ID with a sequential number to create the race.ID
# Create a dataframe for each event with columns for the race weblink, date, start_location, finish_location
# 02_function_obtain_event_race_results_weblinks.R   has the function 'write_race_results_tables'
  
  Race_Weblink_Year <- GetRaceWebLinks(input_year)  
  head(Race_Weblink_Year)
  View(Race_Weblink_Year)
  
# Open each race weblink and extract tables
# 03_function_race_results_table.R

  # Use Windows Progress Bar
  total <- nrow(Race_Weblink_Year)
  # create progress bar
  pb <- winProgressBar(title = paste("Race Table ", input_year, " Download Progress", sep = ""), label = "0% done",  min = 0,
                       max = total, width = 300)
  
  for (r in 1: nrow(Race_Weblink_Year)){
    Sys.sleep(0.1)
    setWinProgressBar(pb, r, title = , label = paste( round(r/total*100, 0),
                                          "% done"))
    write_race_results_tables(Race_Weblink_Year[r, 1], Race_Weblink_Year[r, 2])
    
    # Sleep function to randomise web queries
    sleep <- abs(rnorm(1)) + runif(1, 0, .25)
    message("I have done ", r, " of ", nrow(Race_Weblink_Year),
            " - gonna sleep ", round(sleep, 2),
            " seconds.")
    Sys.sleep(sleep)
    
    }   # End FOR loop to retrieve race results tables using function "write_race_results_tables"
        # 'write_race_results_tables' function comes from 03_functoin_race_results_tables.R
  close(pb)   # Windows Progress Bar script 
  
  
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

