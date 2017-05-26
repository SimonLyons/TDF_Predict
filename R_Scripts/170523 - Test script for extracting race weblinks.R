
# Script to assist in sorting out scraping of race weblink data.
# I've discovered that my script to extract the date works only
# on non-tabulated data data.


library(rvest)
library(dplyr)
library(lubridate)


# Read in 2015 calendar file
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/Calendar_files/")
calendar_CN <- read.csv("race_calendar_2015.csv")

calendar_CN$race_details <- text_clean(calendar_CN$race_details)
calendar_CN$location <- text_clean(calendar_CN$location)

# Convert start and end dates to correct format
calendar_CN$start_date <- dmy(calendar_CN$start_date)
calendar_CN$end_date <- dmy(calendar_CN$end_date)

# Add a dummy race_id
calendar_CN$race_id <- paste("race_id_", 1:length(calendar_CN$race_details), sep = "")
View(calendar_CN)

# Create a subset of the calendar for testing purposes
calendar_CN <- calendar_CN[1:10, ]

# Create a subset of the calendar for testing purposes
# This one includes the Tour De France from 2015 and immediately encountered problems.
# It appears the race link for the TDF goes to some form of "countdown page":
# http://www.cyclingnews.com//races/2015-tour-de-france-countdown-2015
# This appears to be more of an issues for the race calendar function than the race weblinks function.
calendar_CN <- calendar_CN[236:245, ]




#######################################################
# Set up a dummy race calendar data frame.
calendar_CN <- as.data.frame(matrix(data = NA, 3, ncol = 3 ))
colnames(calendar_CN) <- c("web_link", "race_details", "race_id")

# Random stage race
calendar_CN$web_link[1] <- "races/giro-ditalia-2015/stages/"
calendar_CN$race_details[1] <- "Giro d'Italia 2015"
calendar_CN$race_id[1] <- "giro2015r001"


# Races page with table of stages/races:
calendar_CN$web_link[2] <- "races/giro-ditalia-2009/stages/"
calendar_CN$race_details[2] <- "Giro d'Italia 2009"
calendar_CN$race_id[2] <- "giro2009r001"

# Race page with no table:
calendar_CN$web_link[3] <- "races/cycling-australia-road-national-championships-2014-2014/"
calendar_CN$race_details[3] <- "Australia Road National Championships 2014"
calendar_CN$race_id[3] <- "AusRoad001"

# View(calendar_CN)

# Pick one of the races for testing
e <- 3


# Extract relevant weblink and race name
race_url <- paste("http://www.cyclingnews.com/", calendar_CN$web_link[e], sep = "") 
# race_url <- "http://www.cyclingnews.com/races/tour-de-san-luis-2013/"
race_details <- calendar_CN$race_details[e]
race_id <- calendar_CN$race_id[e]

# Download the webpage data
if(RCurl::url.exists(race_url)){
  try(download.file(race_url, "race_url.xml"))
  race_html <- read_html("race_url.xml")}

# Extract the weblinks (which are not in the table data).
# The weblinks also need to be extracted on pages not using a table.
race_links <- race_html %>% 
  html_nodes(xpath="//a[contains(@href, '/results')]") %>% 
  html_attr("href")
# Delete duplicate stage weblinks
race_links <- race_links[!duplicated(race_links)]

# Do a test for the 'table' node to see if a table exists.
# If it does, use the table method to extract the stage dates and other data.
if(length(html_nodes(race_html, xpath="//table")) > 0){
  # Extract information when stored in table
  my_table <- race_html %>% 
    html_nodes(xpath="//table") %>% 
    html_table(fill = TRUE, trim = TRUE) %>% 
    as.data.frame()
  
  # At the moment the table has additional rows
  # Delete rest day rows, where 'results' = ""
  my_table <- my_table %>% 
    filter(results != "")
  # Add race_links to table
  my_table$weblinks <- race_links
  # Convert date values to correct class using lubridate
  my_table$date <- mdy(my_table$date)
  # Convert distance values to correct 'numeric' class
  my_table$distance <- as.numeric(gsub(" km", "", my_table$distance))
  # Remove unecessary columns
  my_table <- my_table %>% select(Stage, date, location, distance, weblinks)
  View(my_table)
}







