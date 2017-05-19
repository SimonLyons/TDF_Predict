# Script to extract relevant elements from Cycling News individual race webite.
# This is the website link I extracted from the calendar page.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

GetRaceWebLinks <- function(input_year){
  
# Load necessary packages
# require(XML)   # I will ideally extract all 'XML' functions and replace with 'rvest'
require(RMySQL)
require(rvest)
require(lubridate)
  
# Set working directory to user passwords location
setwd("C:/b_Data_Analysis/Database")
# Read password file
psswd <- read.csv("passwords_db.csv", header = TRUE)
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]),
                        password = as.character(psswd[psswd$type == "Manager", "password"]),
                        dbname='ProCycling', host='localhost')  
# Read in calendar dataframe from the ProCycling database
query <- dbSendQuery(conn_local, paste("SELECT * FROM race_calendar_", input_year, ";", sep = ""))
# Note the 'n=-1' is required to return all rows, otherwise the database only returns a max of 500.
calendar_CN <- dbFetch(query, n=10)   ##### Set to a limit of 10 rows for testing purposes #######################################

# Create a counter that keeps a running tally of the columns in the master dataframe
# Starts at 1 (obviously) and then adds a row with each new race weblink
col_counter <- 1

# Create an empty master dataframe
races_master <- as.data.frame(matrix(data = NA, 0, ncol = 5 ))
colnames(races_master)[1] <- "stage_url"
colnames(races_master)[2] <- "stage_id"
colnames(races_master)[3] <- "race_id"
colnames(races_master)[4] <- "race_details"
colnames(races_master)[5] <- "stage_date"

# Use Text Progress Bar
total <- nrow(calendar_CN)
# Create text progress bar
prg <- txtProgressBar(min = 0, max = total, style = 3)

# FOR loop to run through all of the events listed in the calendar dataframe
for(e in 1:total){
  # Setup text-based progress bar
  setTxtProgressBar(prg, e)

  # The LOOP needs to ignore events with no weblink
  if (!is.na(calendar_CN$web_link[e])){
    # Extract relevant weblink and race name
    race_url <- paste("http://www.cyclingnews.com/", calendar_CN$web_link[e], sep = "") 
    # race_url <- "http://www.cyclingnews.com/races/tour-de-san-luis-2013/"
    race_details <- calendar_CN$race_details[e]
    race_id <- calendar_CN$race_id[e]

    # Pull in the XML data from the weblink
    # if(RCurl::url.exists(race_url)){
    # race_xml <- htmlParse(race_url)}
    
    # New method avoiding use of XML. Seems more robust.
    if(RCurl::url.exists(race_url)){
    download.file(race_url, "race_url.xml")
    race_html <- read_html("race_url.xml")}
   
    # This line does a good job of isolating the XML attributes containing race web link info
    # No longer used. Replaced with 'rvest' functions below
    # race_links <- xpathApply(race_xml, '//a[contains(@href, "/results")]', xmlAttrs)
    
    # Extract the stage link information using rvest functions
    race_links <- race_html %>% 
      html_nodes(xpath="//a[contains(@href, '/results')]") %>% 
      html_attrs()
    
    # Extract the stage dates. Convert to correct date format using lubridate.
    stage_dates <- race_html %>% 
      html_nodes(xpath="//article/header/time") %>% 
      html_text() %>% 
      mdy()   # Coverts string format dates into date format using Lubridate

    
    # IF statement to only perform extraction if there are results links in the html data
    if (length(race_links) > 0){
      # create single column dataframe to capture webpage links to races
      races_cn <- as.data.frame(matrix(data = NA, nrow = length(race_links), ncol = 5 ))
      colnames(races_cn)[1] <- "stage_url"
      colnames(races_cn)[2] <- "stage_id"
      colnames(races_cn)[3] <- "race_id"
      colnames(races_cn)[4] <- "race_details"
      colnames(races_cn)[5] <- "stage_date"
      
      
      # Run LOOP to pull out the weblink to the results page
      # This actually took me ages to isolate just the 'href' value
      # Currently has an error thanks to the stage date information being a shorter
      # string that the stage links information. (Duplicate stage links data)
      for(n in 1: length(race_links)){
        races_cn[n,1] <- race_links[[n]][[name = "href"]]
        races_cn[n,2] <- paste(race_id, "_s", formatC(n, width = 2, format = "d", flag = "0"), sep = "")
        races_cn[n,3] <- as.character(race_id)
        races_cn[n,4] <- as.character(race_details)
        races_cn[n,5] <- stage_dates[n]
      }   # End FOR loop (n) that pulls out weblink data
      
      # Remove duplicate entries
      races_cn <- races_cn[!duplicated(races_cn[,1]), ]  
      
      # Force date column to class 'date'
      races_cn$stage_date <- as_date(races_cn$stage_date)
      
      # Row bind the small races_cn dataframe to the races_master dataframe
      races_master <- rbind(races_master, races_cn)
      
      races_master$stage_date <- as_date(races_master$stage_date)
     
    }   #  END IF statement relating to whether race links exist in the HTML data
  }   # End IF statement identifying whether a race result link exists for each calendar entry
  
  # Insert sleep script to randomise web queries
  sleep <- abs(rnorm(1)) + runif(1, 0, .25)
  Sys.sleep(sleep)
  
}   #   End FOR loop 'e' to run through all of the events in the calendar dataframe

close(prg)   # End txt progress bar
# View(races_master)

# Write 'race_master' dataframe to ProCycling database
dbWriteTable(conn_local,type = 'UTF-8', name = paste("race_weblinks_", input_year, sep = ""), races_master, 
             overwrite = TRUE, row.names = FALSE)
return(races_master)

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

}   # End function GetRaceWebLinks
