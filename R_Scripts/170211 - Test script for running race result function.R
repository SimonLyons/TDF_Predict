# Master script for calling functions to extract race results data
# from the Cycling News


#################################################
# Create files for CN race calendars
# This essentially only needs to be done once.
# 
# Define range of years. Can be modified below.
start_year <- 2021
end_year <- 2021

# Run 'initialCNCalendar' function
initialCNCalendar(start_year, end_year)

# Run calendar conversion function 
CalendarAddAndClean(start_year, end_year)

#################################################


# Function to go and extract all of the race results tables for an entire calendar year
# Define the year
input_year <- 2021

# Begin function
GetAllRacesInAYear <- function(input_year){

# Read in CN calendar csv file
# calendar_CN_year <- read.csv(paste("calendar_CN_", input_year, "_final.csv", sep = ""), header = TRUE, sep = ","))

# Extract (into dataframe) the following columns Web.link	Start.date	End.date	event.name	event.ID

# Go to each Web.link and extract the race web links for each event.ID. Create race.ID
# So we'll combine the event.ID with a sequential number to create the race.ID
# Create a dataframe for each event with columns for the race weblink, date, start.location, finish.location
# 02_function_obtain_event_race_results_weblinks.R   has the function 'write_race_results_tables'
  
  Race_Weblink_Year <- GetRaceWebLinks(input_year)  
  head(Race_Weblink_Year)
  
# Open each race weblink and extract tables
# 03_function_race_results_table.R

  # Use Windows Progress Bar
  total <- nrow(Race_Weblink_Year)
  # create progress bar
  pb <- winProgressBar(title = "Race Table Download Progress", label = "0% done",  min = 0,
                       max = total, width = 300)
  
  for (r in 1: nrow(Race_Weblink_Year)){
    Sys.sleep(0.1)
    setWinProgressBar(pb, r, title = , label = paste( round(r/total*100, 0),
                                          "% done"))
    write_race_results_tables(Race_Weblink_Year[r, 1], Race_Weblink_Year[r, 2])
    }   # End FOR loop to retrieve race results tables using function "write_race_results_tables"
  close(pb)   # Windows Progress Bar script 
  
  
# Create a dataframe for each race result table. Name it for the race.ID

}   # End overall function 'GetAllRacesInAYear'



