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
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Designer", "user"]) , password = as.character(psswd[psswd$type == "Designer", "password"]),  dbname='ProCycling', host='localhost')
  
# Read in calendar dataframe from the ProCycling database
query <- dbSendQuery(conn_local, paste("SELECT * FROM race_calendar_", input_year, ";", sep = ""))
calendar_CN <- dbFetch(query, n=-1)   # Note the 'n=-1' is required to return all rows, otherwise the database only returns a max of 500!!
  
# Create a counter that keeps a running tally of the columns in the master dataframe
# Starts at 1 (obviously) and then adds a row with each new race weblink
col_counter <- 1

# Create an empty master dataframe
# create single column dataframe to capture webpage links to races
races_master <- as.data.frame(matrix(data = NA, 0, ncol = 4 ))
colnames(races_master)[1] <- "race_url"
colnames(races_master)[2] <- "race_id"
colnames(races_master)[3] <- "event_id"
colnames(races_master)[4] <- "event_name"


# Use Windows Progress Bar
total <- nrow(calendar_CN)
# create progress bar
pb <- winProgressBar(title = paste("Obtain race_name & race_id from race_calendar_", input_year, sep = ""), label = "0% done", min = 0,
                     max = total, width = 300)

# FOR loop to run through all of the events listed in the calendar dataframe
# The LOOP needs to ignore events with no weblink
for(e in 1:nrow(calendar_CN)){
  
  Sys.sleep(0.1)   # Windows Progress Bar script
  setWinProgressBar(pb, e, title = "Obtain event.name & event.ID from calendar", label=paste( round(e/total*100, 0),
                                        "% done"))
  
  if (!is.na(calendar_CN$web_link[e])){
    # Extract relevant weblink and race name
    race_url <- paste("http://www.cyclingnews.com/", calendar_CN$web_link[e], sep = "") 
    race_name <- calendar_CN$race_name[e]
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
      colnames(races_cn)[4] <- "race_name"
      
      # Run LOOP to pull out the weblink to the results page
      # This actually took me ages to isolate just the 'href' value
      for(n in 1: length(race_links)){
        races_cn[n,1] <- race_links[[n]][[name = "href"]]
        races_cn[n,2] <- paste(race_id, "_s", formatC(n, width = 2, format = "d", flag = "0"), sep = "")
        races_cn[n,3] <- as.character(race_id)
        races_cn[n,4] <- as.character(race_name)
      }   # End FOR loop that pulls out weblink data
      
      # Remove duplicate entries
      races_cn <- races_cn[!duplicated(races_cn[,1]), ]  
      
      # Row bind the small races_cn dataframe to the races_master dataframe
      races_master <- rbind(races_master, races_cn)
     
    }   #  END IF statement relating to whether race links exist in the HTML data
  }   # End IF statement identifying whether a race result link exists for each calendar entry
  
  # Insert sleep script to randomise web queries
  sleep <- abs(rnorm(1)) + runif(1, 0, .25)
  message("I have done ", e, " of ", nrow(calendar_CN),
          " - gonna sleep ", round(sleep, 2),
          " seconds.")
  Sys.sleep(sleep)
  
}   #   End FOR loop to run through all of the events in the calendar dataframe

close(pb)   # Windows Progress Bar script

#################
# Cleanup script takes the initial race calendar and peforms the following actions:
#    1. Adds columns for 'race_name' and 'race_id'
#    2. Converts latin and other non 'UTF-8' characters
# Script to take races_master, replace latin characters from the race details column
# and use the clean name to create a new race_name column and unique race ID column

# Add column to dataframe for race_id
races_master$race_id <- NA

for (i in 1:nrow(races_master)){
  # First create clean race name
  races_master[i, "race_details"] <- removeDiscritics(races_master[i, "race_details"])
  races_master[i, "race_details"] <- gsub("/", "", races_master[i, "race_details"])
  races_master[i, "race_details"] <- gsub(":", "", races_master[i, "race_details"])
  races_master[i, "race_details"] <- gsub("’", "'", races_master[i, "race_details"])
  # races_master[i, "race_details"] <- enc2utf8(races_master[i, "race_details"])
  # Next clean the race location
  races_master[i, "location"] <- removeDiscritics(races_master[i, "location"])
  races_master[i, "location"] <- as.character(races_master[i, "location"])
  # races_master[i, "location"] <- enc2utf8(races_master[i, "location"])
  # Race ID of format race_YYYY_000N.
  # Updated to simple sequential numbering. Was previously combination of year and race name.
  races_master[i, "race_id"] <- paste("race", n, formatC(i, width = 4, format = "d", flag = "0"),  sep = "_" )
}

# Encoding(races_master$race_details[71]) <- 'UTF-8'
races_master$race_details <- as.character(races_master$race_details)
races_master$race_details <- enc2utf8(races_master$race_details)
races_master$location <- as.character(races_master$location)
races_master$location <- enc2utf8(races_master$location)


# Write 'race_master' dataframe to ProCycling database
dbWriteTable(conn_local,type = 'UTF-8', name = paste("race_weblinks_", input_year, sep = ""), races_master, overwrite = TRUE)
return(races_master)

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

}   # End function GetRaceWebLinks
