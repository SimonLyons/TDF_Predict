# Initial function 'initialCNCalendar' to read the race calendar data from the Cycling News website
# Function requires a 'start_year' and 'end_year' as inputs

initialCNCalendar <- function(start_year, end_year){

# Script to extract relevant elements from Cycling News calendar webite.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

# Call relevant libraries
# In this case, just 'XML' for webscraping
  require(XML)
  require(RMySQL)

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
  colnames(calendar_cn)[1] <- "race_date"
  colnames(calendar_cn)[2] <- "race_details"
  colnames(calendar_cn)[3] <- "discipline"
  colnames(calendar_cn)[4] <- "location"
  colnames(calendar_cn)[5] <- "uci_code"
  colnames(calendar_cn)[6] <- "web_link"
  colnames(calendar_cn)[7] <- "start_date"
  colnames(calendar_cn)[8] <- "end_date"
  
  # FOR loop to run through the number of row entries in the calendar table
  for(j in 1:length(td_ns)){
    my_r1 <- td_ns[[j]]
    
    # FOR loop to run through each column (for each row) and input the relevant XML attribute
    for(i in 1:5){
      calendar_cn[j, i] <- (xmlValue(my_r1[[i]]))
      
      calendar_cn[j,"race_date"] <- gsub("\t", "", calendar_cn[j,"race_date"])
      calendar_cn[j,"race_date"] <- gsub("\n", "", calendar_cn[j,"race_date"])
      
      
    } # End 'i' loop to run over calendar columns
    
    # IF statement to check for existence of web link
    if(!is.null(xmlAttrs(my_r1[[2]][[1]]))){
      calendar_cn[j, 6] <- paste(xmlAttrs(my_r1[[2]][[1]]), sep = "")
      }   # End if statement
    
    # IF statement to add START and END columns for the date
    if(any(grepl(" to ", calendar_cn[j, "race_date"]))){
      pos_to <- regexpr(" to ", calendar_cn[j, "race_date"])[1]   # Determine text position of ' to '
      calendar_cn[j, "start_date"] <- paste(substr(calendar_cn[j, "race_date"],1  ,  pos_to-1), n, sep = " ")
      calendar_cn[j, "end_date"] <- paste(substr(calendar_cn[j, "race_date"],pos_to +4  ,  nchar(calendar_cn[j, "race_date"])), n, sep = " ")
    }   else {
      calendar_cn[j, "start_date"] <- paste(calendar_cn[j, "race_date"], n, sep = " ")
      calendar_cn[j, "end_date"] <- NA
      
    }   # End IF statement
    
  } # End 'j' loop to run over calendar rows
    
  # Remove "\t" and "\n" from race_details column
  calendar_cn$race_details <- gsub("\t", "", calendar_cn$race_details)
  calendar_cn$race_details <- gsub("\n", "", calendar_cn$race_details)

  # Create a dataframe with the table information and append it with the YEAR
  assign(paste("race_calendar_", n, sep = ""), calendar_cn)
  
  # Write CSV file for each calendar
  write.csv(assign(paste("race_calendar_", n, sep = ""), calendar_cn), file = paste("race_calendar_", n, ".csv", sep = ""), row.names = FALSE)

}   # End loop for calendars between 'start_year' and 'end_year'

}   # End 'initialCNCalendar' function




