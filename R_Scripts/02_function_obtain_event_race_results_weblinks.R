# Script to extract relevant elements from Cycling News individual race webite.
# This is the website link I extracted from the calendar page.
# This method uses xpathApply to look at the nodes and divide it into relevant rows


GetRaceWebLinks <- function(input_year){

# Load necessary packages
require(XML)

# set working directory
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

# Read in the calendar .csv file.
calendar_CN <- read.csv(paste("calendar_CN_", input_year, "_final.csv", sep = ""), header = TRUE, sep = ",")

# Create a counter that keeps a running tally of the columns in the master dataframe
# Starts at 1 (obviously) and then adds a row with each new race weblink
col_counter <- 1

# Create an empty master dataframe
# create single column dataframe to capture webpage links to races
races_master <- as.data.frame(matrix(data = NA, 0, ncol = 4 ))
colnames(races_master)[1] <- "race.url"
colnames(races_master)[2] <- "race.ID"
colnames(races_master)[3] <- "event.ID"
colnames(races_master)[4] <- "event.name"


# Use Windows Progress Bar
total <- nrow(calendar_CN)
# create progress bar
pb <- winProgressBar(title = "Obtain event.name & event.ID from calendar", label = "0% done", min = 0,
                     max = total, width = 300)

# FOR loop to run through all of the events listed in the calendar dataframe
# The LOOP needs to ignore events with no weblink
for(e in 1:nrow(calendar_CN)){
  
  Sys.sleep(0.1)   # Windows Progress Bar script
  setWinProgressBar(pb, e, title = "Obtain event.name & event.ID from calendar", label=paste( round(e/total*100, 0),
                                        "% done"))
  
  if (!is.na(calendar_CN$Web.link[e])){
    # Extract relevant weblink and race name
    race_url <- paste("http://www.cyclingnews.com/", calendar_CN$Web.link[e], sep = "") 
    event_name <- calendar_CN$event.name[e]
    event_ID <- calendar_CN$event.ID[e]
    
    # Pull in the XML data from the weblink
    race_xml <- htmlParse(race_url)
    
    # This line does a good job of isolating the XML attributes containing race web link info
    race_links <- xpathApply(race_xml, '//a[contains(@href, "/results")]', xmlAttrs)
    
    # IF statement to only perform extraction if there are results links in the html data
    if (length(race_links) > 0){
      
      # create single column dataframe to capture webpage links to races
      races_cn <- as.data.frame(matrix(data = NA, nrow = length(race_links), ncol = 4 ))
      colnames(races_cn)[1] <- "race.url"
      colnames(races_cn)[2] <- "race.ID"
      colnames(races_cn)[3] <- "event.ID"
      colnames(races_cn)[4] <- "event.name"
      
      # Run LOOP to pull out the weblink to the results page
      # This actually took me ages to isolate just the 'href' value
      for(n in 1: length(race_links)){
        races_cn[n,1] <- race_links[[n]][[name = "href"]]
        races_cn[n,2] <- paste(event_ID, ".R", formatC(n, width = 2, format = "d", flag = "0"), sep = "")
        races_cn[n,3] <- as.character(event_ID)
        races_cn[n,4] <- as.character(event_name)
      }   # End FOR loop that pulls out weblink data
      
      # Remove duplicate entries
      races_cn <- races_cn[!duplicated(races_cn[,1]), ]  
      
      # Row bind the small races_cn dataframe to the races_master dataframe
      races_master <- rbind(races_master, races_cn)
      
      # Write CSV file for each calendar
      # write.csv(races_cn, file = paste(event_ID, "_EventTable", ".csv", sep = ""), row.names = FALSE)
      
    }   #  END IF statement relating to whether race links exist in the HTML data
  }   # End IF statement identifying whether a race result link exists for each calendar entry
  
}   #   End FOR loop to run through all of the events in the calendar dataframe

close(pb)   # Windows Progress Bar script

write.csv(races_master, file = paste(input_year, "CN_race_weblinks", ".csv", sep = ""), row.names = FALSE)
return(races_master)

}   # End function GetRaceWebLinks
