# Script to extract relevant elements from Cycling News individual race webite.
# This is the website link I extracted from the calendar page.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

GetRaceWebLinks <- function(input_year){
  
# Load necessary packages
require(XML)
require(RMySQL)
  
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
calendar_CN <- dbFetch(query, n=-1)   

# Create a counter that keeps a running tally of the columns in the master dataframe
# Starts at 1 (obviously) and then adds a row with each new race weblink
col_counter <- 1

# Create an empty master dataframe
races_master <- as.data.frame(matrix(data = NA, 0, ncol = 4 ))
colnames(races_master)[1] <- "stage_url"
colnames(races_master)[2] <- "stage_id"
colnames(races_master)[3] <- "race_id"
colnames(races_master)[4] <- "race_details"

# Use Text Progress Bar
total <- nrow(calendar_CN)
# Create text progress bar
prg <- txtProgressBar(min = 0, max = total, style = 3)

# FOR loop to run through all of the events listed in the calendar dataframe
for(e in 1:total){
  # Setup text-based progress bar
  setTxtProgressBar(prg, e,)

    # The LOOP needs to ignore events with no weblink
  if (!is.na(calendar_CN$web_link[e])){
    # Extract relevant weblink and race name
    race_url <- paste("http://www.cyclingnews.com/", calendar_CN$web_link[e], sep = "") 
    race_details <- calendar_CN$race_details[e]
    race_id <- calendar_CN$race_id[e]

    # Pull in the XML data from the weblink
    race_xml <- htmlParse(race_url)
    
    # This line does a good job of isolating the XML attributes containing race web link info
    race_links <- xpathApply(race_xml, '//a[contains(@href, "/results")]', xmlAttrs)
    
    # IF statement to only perform extraction if there are results links in the html data
    if (length(race_links) > 0){
      # create single column dataframe to capture webpage links to races
      races_cn <- as.data.frame(matrix(data = NA, nrow = length(race_links), ncol = 4 ))
      colnames(races_cn)[1] <- "stage_url"
      colnames(races_cn)[2] <- "stage_id"
      colnames(races_cn)[3] <- "race_id"
      colnames(races_cn)[4] <- "race_details"
      
      # Run LOOP to pull out the weblink to the results page
      # This actually took me ages to isolate just the 'href' value
      for(n in 1: length(race_links)){
        races_cn[n,1] <- race_links[[n]][[name = "href"]]
        races_cn[n,2] <- paste(race_id, "_s", formatC(n, width = 2, format = "d", flag = "0"), sep = "")
        races_cn[n,3] <- as.character(race_id)
        races_cn[n,4] <- as.character(race_details)
      }   # End FOR loop (n) that pulls out weblink data
      
      # Remove duplicate entries
      races_cn <- races_cn[!duplicated(races_cn[,1]), ]  
      
      # Row bind the small races_cn dataframe to the races_master dataframe
      races_master <- rbind(races_master, races_cn)
     
    }   #  END IF statement relating to whether race links exist in the HTML data
  }   # End IF statement identifying whether a race result link exists for each calendar entry
  
  # Insert sleep script to randomise web queries
  sleep <- abs(rnorm(1)) + runif(1, 0, .25)
  Sys.sleep(sleep)
  
}   #   End FOR loop 'e' to run through all of the events in the calendar dataframe

close(prg)   # End txt progress bar
View(races_master)

# Write 'race_master' dataframe to ProCycling database
dbWriteTable(conn_local,type = 'UTF-8', name = paste("race_weblinks_", input_year, sep = ""), races_master, overwrite = TRUE)
return(races_master)

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

}   # End function GetRaceWebLinks
