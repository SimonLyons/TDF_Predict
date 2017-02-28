# Cleanup function 'CalendarAddAndClean' takes the initial race calendar and peforms the following actions:
#    1. Adds columns for 'race_name' and 'race_id'
#    2. Converts latin and other non 'UTF-8' characters
# Function requires a 'start_year' and 'end_year' as inputs

CalendarAddAndClean <- function(start_year, end_year){

# Script to take CN calendar, replace latin characters from the race details column
# and use the clean name to create a new race_name column and unique race ID column

require(RMySQL)   # Package required for writing tables to database
  
# set working directory
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

# Create connection to Procycling database
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='localhost')

# Use function to replace latin and foreign characters with basic ASCII characters
removeDiscritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ"
    ,"SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy"
    , string
  )
}


for (n in start_year:end_year){

  # Read in calendar .csv file
  add_ID_df <- read.csv(paste("race_calendar_", n, ".csv", sep = ""), header = TRUE)
  # Add columns to dataframe for race_name and race_id
  add_ID_df$race_name <- NA
  add_ID_df$race_id <- NA

  for (i in 1:nrow(add_ID_df)){
    # First create clean race name
    add_ID_df[i, "race_name"] <- removeDiscritics(add_ID_df[i, "race_details"])
    add_ID_df[i, "race_name"] <- gsub("/", "", add_ID_df[i, "race_name"])
    add_ID_df[i, "race_name"] <- gsub(":", "", add_ID_df[i, "race_name"])
    # Race ID of format race_YYYY_000N.
    # Updated to simple sequential numbering. Was previously combination of year and race name.
    add_ID_df[i, "race_id"] <- paste("race", n, formatC(i, width = 4, format = "d", flag = "0"),  sep = "_" )
  }

# Write CSV file for each calendar
# write.csv(assign(paste("race_calendar_", n, sep = ""), add_ID_df), file = paste("race_calendar_", n, "_final.csv", sep = ""), row.names = FALSE)

# write table to ProCycling database
dbWriteTable(conn_local, name = paste("race_calendar_", n, sep = ""), add_ID_df[ , -2], overwrite = TRUE) #, "race_id" INTEGER PRIMARY KEY)

}   # End loop through yearly calendars

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

}   # End function 'CalendarAddAndClean'
