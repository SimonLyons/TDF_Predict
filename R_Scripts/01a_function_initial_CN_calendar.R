# Initial function 'initialCNCalendar' to read the race calendar data from the Cycling News website
# Function requires a 'start_year' and 'end_year' as inputs

initialCNCalendar <- function(start_year, end_year){

# Script to extract relevant elements from Cycling News calendar webite.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

# Call relevant libraries
  require(XML)
  require(RMySQL)
# set working directory
  setwd("C:/b_Data_Analysis/Database")

# Read password file
  psswd <- read.csv("passwords_db.csv", header = TRUE)
  conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]),
                          password = as.character(psswd[psswd$type == "Manager", "password"]),
                          dbname='ProCycling', host='localhost')

# Need to replace this progress bar with the one used in our Ozdata files at UnConf 17
# Use Windows Progress Bar
total <- length(start_year:end_year)
# create progress bar
pb <- winProgressBar(title = paste("Obtain race calendar for year ", input_year, sep = ""), label = "0% done", min = 0,
                     max = total, width = 300)

# Create a list of the calendars being written to the database
# This is the empty list to be populated at the end of the loop
calendar_list <- c()
  
# Create FOR loop that creates a web address for Cycling News calendars between 2005 and 2017
for (n in start_year:end_year){
  
  Sys.sleep(0.1)   # Windows Progress Bar script
  setWinProgressBar(pb, (n-start_year+1), paste("Obtain race calendar for year ", input_year, sep = ""), label=paste( round((n-start_year+1)/total*100, 0),
                                                                                              "% done"))
  
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
  for(j in 1:30){  # length(td_ns)){
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
    
    # Insert sleep script to randomise web queries
    sleep <- abs(rnorm(1)) + runif(1, 0, .25)
    message("I have done ", j, " of ", length(td_ns),
            " - gonna sleep ", round(sleep, 2),
            " seconds.")
    Sys.sleep(sleep)
    
  } # End 'j' loop to run over calendar rows
    
  # Remove "\t" and "\n" from race_details column
  calendar_cn$race_details <- gsub("\t", "", calendar_cn$race_details)
  calendar_cn$race_details <- gsub("\n", "", calendar_cn$race_details)

  # Add column to dataframe for race_id
  calendar_cn$race_id <- NA
  
  for (i in 1:nrow(calendar_cn)){
    # First create clean race name/details
    calendar_cn[i, "race_details"] <- removeDiscritics(calendar_cn[i, "race_details"])
    calendar_cn[i, "race_details"] <- removePainfulCharacters(calendar_cn[i, "race_details"])
    calendar_cn[i, "race_details"] <- gsub("[[:punct:]]", "", calendar_cn[i, "race_details"])
    # The following line removes the 'non-breaking space' character. Very annoying!!
    calendar_cn[i, "race_details"] <- gsub(rawToChar(as.raw("0xa0")), "", calendar_cn[i, "race_details"])
    calendar_cn[i, "race_details"] <- gsub("  ", " ", calendar_cn[i, "race_details"])

    # Next clean the race location
    calendar_cn[i, "location"] <- removeDiscritics(calendar_cn[i, "location"])
    calendar_cn[i, "location"] <- as.character(calendar_cn[i, "location"])
    calendar_cn[i, "location"] <- gsub("[[:punct:]]", "", calendar_cn[i, "location"])
    # The following line removes the 'non-breaking space' character. Very annoying!!
    calendar_cn[i, "location"] <- gsub(rawToChar(as.raw("0xa0")), "", calendar_cn[i, "location"])
    calendar_cn[i, "location"] <- gsub("  ", " ", calendar_cn[i, "location"])
    
    # Race ID of format race_YYYY_000N.
    # Updated to simple sequential numbering. Was previously combination of year and race name.
    calendar_cn[i, "race_id"] <- paste("race", n, formatC(i, width = 4, format = "d", flag = "0"),  sep = "_" )
  }

  # Write 'race_calendar' dataframe to ProCycling database
  try(dbWriteTable(conn_local,type = 'UTF-8', name = paste("race_calendar_", n, sep = ""), calendar_cn, overwrite = TRUE))
  
  # add the lastest race_calendar name to the list being built
  # This list will be returned by the function
  calendar_list <- c(calendar_list, paste("race_calendar_", n, sep = ""))
  
}   # End loop for calendars between 'start_year' and 'end_year'

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

close(pb)   # Windows Progress Bar script

# return(calendar_list)
return(calendar_cn)

}   # End 'initialCNCalendar' function




