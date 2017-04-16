# Script to extract relevant elements from Cycling News individual race webite.
# This is the website link I extracted from the calendar page.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

library(XML)

# set working directory
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

# Read in the calendar .csv file.
calendar_CN <- read.csv("calendar_CN_2009_test.csv", header = TRUE, sep = ",")

# FOR loop to run through all of the events listed in the calendar dataframe
# The LOOP needs to ignore events with no weblink
for(e in 1:nrow(calendar_CN)){

  if (!is.na(calendar_CN$Web.link[e])){
    # Extract relevant weblink and race name for first race (Bpost bank trofee - GP Sven Nys)
    race_url <- calendar_CN$Web.link[e]
    event_name <- calendar_CN$event.name[e]
    event_ID <- calendar_CN$event.ID[e]
    
    # Pull in the XML data from the weblink
    race_xml <- htmlParse(race_url)
    
    # This line does a good job of isolating the XML attributes containing race web link info
    race_links <- xpathApply(race_xml, '//a[contains(@href, "/results")]', xmlAttrs)
    
    # IF statement to only perform extraction if there are results links in the html data
    if (length(race_links) > 0){

      # create single column dataframe to capture webpage links to races
      races_cn <- as.data.frame(matrix(data = NA, nrow = length(race_links), ncol = 2 ))
      colnames(races_cn)[1] <- "race.url"
      colnames(races_cn)[2] <- "race.ID"
      
      # Run LOOP to pull out the weblink to the results page
      # This actually took me ages to isolate just the 'href' value
      for(n in 1: length(race_links)){
        races_cn[n,1] <- race_links[[n]][[name = "href"]]
        races_cn[n,2] <- paste(event_ID, "_", n, sep = "")
        }   # End FOR loop that pulls out weblink data
      # Remove duplicate entries
      races_cn <- unique(races_cn)
      
      # Write CSV file for each calendar
      write.csv(races_cn, file = paste(event_ID, "_EventTable", ".csv", sep = ""), row.names = FALSE)
      
      }   #  END IF statement relating to whether race links exist in the HTML data
      }   # End IF statement identifying whether a race result link exists for each calendar entry

}   #   End FOR loop to run through all of the events in the calendar dataframe
