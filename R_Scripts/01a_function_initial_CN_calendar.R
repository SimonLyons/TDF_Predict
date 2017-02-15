# Initial function 'initialCNCalendar' to read the race calendar data from the Cycling News website
# Function requires a 'start_year' and 'end_year' as inputs

initialCNCalendar <- function(start_year, end_year){

# Script to extract relevant elements from Cycling News calendar webite.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

# Call relevant libraries
# In this case, just 'XML' for webscraping
library(XML)

# set working directory
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

# Create FOR loop that creates a web address for Cycling News calendars between 2005 and 2016
for (n in start_year:end_year){
  my_url <- paste("http://www.cyclingnews.com/races/calendar/", n, sep = "")
  my_url_parse <- htmlParse(my_url)
  table_no <- length(readHTMLTable(my_url_parse))   #   determine number of tables on url
  
  # Divide the xml data into row elements, divided by /tr node with 'data-i' inluded
  td_ns <- xpathApply(my_url_parse, "//tr[@data-i]")
  
  # Create blank calendar table
  calendar_cn <- as.data.frame(matrix(NA, nrow = 0, ncol = 8))
  colnames(calendar_cn)[1] <- "Date"
  colnames(calendar_cn)[2] <- "Race.Details"
  colnames(calendar_cn)[3] <- "Discipline"
  colnames(calendar_cn)[4] <- "Location"
  colnames(calendar_cn)[5] <- "UCI.code"
  colnames(calendar_cn)[6] <- "Web.link"
  colnames(calendar_cn)[7] <- "Start.date"
  colnames(calendar_cn)[8] <- "End.date"
  
  # FOR loop to run through the number of row entries in the calendar table
  for(j in 1:length(td_ns)){
    my_r1 <- td_ns[[j]]
    
    # FOR loop to run through each column (for each row) and input the relevant XML attribute
    for(i in 1:5){
      calendar_cn[j, i] <- (xmlValue(my_r1[[i]]))
      
      calendar_cn[j,"Date"] <- gsub("\t", "", calendar_cn[j,"Date"])
      calendar_cn[j,"Date"] <- gsub("\n", "", calendar_cn[j,"Date"])
      
      
    } # End 'i' loop to run over calendar columns
    
    # IF statement to check for existence of web link
    if(!is.null(xmlAttrs(my_r1[[2]][[1]]))){
      calendar_cn[j, 6] <- paste(xmlAttrs(my_r1[[2]][[1]]), sep = "")
      }   # End if statement
    
    # IF statement to add START and END columns for the date
    if(any(grepl(" to ", calendar_cn[j, "Date"]))){
      pos_to <- regexpr(" to ", calendar_cn[j, "Date"])[1]   # Determine text position of ' to '
      calendar_cn[j, "Start.date"] <- paste(substr(calendar_cn[j, "Date"],1  ,  pos_to-1), n, sep = " ")
      calendar_cn[j, "End.date"] <- paste(substr(calendar_cn[j, "Date"],pos_to +4  ,  nchar(calendar_cn[j, "Date"])), n, sep = " ")
    }   else {
      calendar_cn[j, "Start.date"] <- paste(calendar_cn[j, "Date"], n, sep = " ")
      calendar_cn[j, "End.date"] <- NA
      
    }   # End IF statement
    
  } # End 'j' loop to run over calendar rows
    
  # Remove "\t" and "\n" from Race.Details column
  calendar_cn$Race.Details <- gsub("\t", "", calendar_cn$Race.Details)
  calendar_cn$Race.Details <- gsub("\n", "", calendar_cn$Race.Details)

  # Create a dataframe with the table information and append it with the YEAR
  assign(paste("calendar_CN_", n, sep = ""), calendar_cn)
  
  # Write CSV file for each calendar
  write.csv(assign(paste("calendar_CN_", n, sep = ""), calendar_cn), file = paste("calendar_CN_", n, ".csv", sep = ""), row.names = FALSE)

}   # End loop for calendars between 'start_year' and 'end_year'

}   # End 'initialCNCalendar' function



