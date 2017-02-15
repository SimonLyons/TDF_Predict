

# Create files for CN race calendars
# Define range of years. Can be modified below.
start_year <- 2015
end_year <- 2016

# Run 'initialCNCalendar' function
initialCNCalendar(start_year, end_year)

# Run calendar conversion function 
CalendarAddAndClean(start_year, end_year)


# Function to go and extract all of the race results tables for an entire calendar year
# Define the year
input_year <- 2016

# Begin function
GetAllRacesInAYear <- function(input_year){

# Read in CN calendar csv file
calendar_CN_year <- read.csv(paste("calendar_CN_", input_year, "_final.csv", header = TRUE, sep = ","))

# Extract (into dataframe) the following columns Web.link	Start.date	End.date	event.name	event.ID

event_no <- nrows



# Go to each Web.link and extract the race web links for each event.ID. Create race.ID
# At present I think the easiest thing to do will be to assign a sequential number for the race.ID
# So we'll combine the event.ID with a sequential number to create the race.ID
# Create a dataframe for each event with columns for the race weblink, date, start.location, finish.location
# Currently script for this is in 170207 cyclingnews race webscrape v1.R



# Open each race weblink and extract tables
# Currently this is acheived with the function 'write_race_results_tables' in the following file
# 03_function_race_results_table.R



# Create a dataframe for each race result table. Name it for the race.ID




}   # End overall function 'GetAllRacesInAYear'




# Practice script for testing out bits.
# On Tue 14 Feb I was attempting to scrape tables (unsuccessfully) from race.ID 2009FeniouxFranceTrophy_1
# Different types of tables are causing problems for my getCNresults function

test_urls <- read.csv("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files/calendar_CN_2009FeniouxFranceTrophy.csv", header = TRUE, sep = ",")

url_race1 <- paste("http://www.cyclingnews.com/", as.character(test_urls[1,1]), sep = "")
url_race2 <- paste("http://www.cyclingnews.com/", as.character(test_urls[2,1]), sep = "")

url_race3 <- "http://www.cyclingnews.com/races/dubai-tour-2017/stage-4/results/"
url_race4 <- "http://www.cyclingnews.com/races/dubai-tour-2017/stage-2/results/"


url_race5 <- "http://www.cyclingnews.com/races/giro-ditalia-internazionale-femminile-2016/stage-3/results"
url_race6 <- "http://www.cyclingnews.com/races/fenioux-france-trophy-isgp3/sprint/results"


race.ID <- as.character(test_urls[1,2])
race.ID <- "dubai_2"

tables_out <- write_race_results_tables(url_race4, race.ID)
tables_out

my_url <- url_race4

table_no <- length(readHTMLTable(url_race1))
my_results <- as.data.frame(readHTMLTable(url_race1)[1])
ncol(my_results)


my_url <- url_race1



