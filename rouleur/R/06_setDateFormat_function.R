
# Create function to convert webscraped dates to correct class
# Use Hadley's 'lubridate' package

setDateFormat <- function(start_year, end_year){
  require(lubridate)
  require(RMySQL)
  
  # Connect to ProCyling database
  setwd("C:/b_Data_Analysis/Database")

  # Read password file
  psswd <- read.csv("passwords_db.csv", header = TRUE)
  conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , password = as.character(psswd[psswd$type == "Manager", "password"]),  dbname='ProCycling', host='localhost')

  # dbListTables(conn_local, dbname='ProCycling')


  for(c in start_year:end_year){
    # fetch race calendar table (for specified year in loop) from ProCycling database
    query <- dbSendQuery(conn_local, paste0("SELECT * FROM race_calendar", c, ";"))
    calendar_cn <- dbFetch(query, n=-1)
    
    # Convert both 'start_date' and 'end_date' to correct date format
    calendar_cn$start_date <- dmy(calendar_cn$start_date)
    calendar_cn$end_date <- dmy(calendar_cn$end_date)
    # Delete the row_names column that is apparently imported from the database table
    calendar_cn <- calendar_cn[ , -1]
    
    # Write 'race_calendar' dataframe to ProCycling database
    dbWriteTable(conn_local,type = 'UTF-8', name = paste("race_calendar_", c, sep = ""), calendar_cn, overwrite = TRUE)
    # Close current connection (for query)
    dbDisconnect(dbListConnections(MySQL())[[1]])
  
    }   # End loop to run through specified years

  # Script for closing all active connections to MySQL databases.
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) 
    dbDisconnect(con)
  
}   # End 'setDateFormat' function
