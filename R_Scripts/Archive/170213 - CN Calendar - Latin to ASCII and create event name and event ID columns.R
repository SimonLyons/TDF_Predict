# Script to take CN calendar, replace latin characters from the race details column
# and use the clean name to create a new event.name column and unique event ID column


# set working directory
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

# Use function to replace latin and foreign characters with basic ASCII characters
removeDiscritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ"
    ,"SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy"
    , string
  )
}


for (n in 2005:2016){

  # Read in calendar .csv file
  add_ID_df <- read.csv(paste("calendar_CN_", n, ".csv", sep = ""), header = TRUE)
  # Add columns to dataframe for event.name and event.ID
  add_ID_df$event.name <- NA
  add_ID_df$event.ID <- NA

  for (i in 1:nrow(add_ID_df)){
    # First create clean event name
    add_ID_df[i, "event.name"] <- removeDiscritics(add_ID_df[i, "Race.Details"])
    add_ID_df[i, "event.name"] <- gsub("/", "", add_ID_df[i, "event.name"])
    add_ID_df[i, "event.name"] <- gsub(":", "", add_ID_df[i, "event.name"])
    # Next create unique event ID by removing spaces and appending year
    add_ID_df[i, "event.ID"] <- gsub(" ", "", add_ID_df[i, "event.name"])
    add_ID_df[i, "event.ID"] <- paste(n, add_ID_df[i, "event.ID"], sep = "" )
  }

# Write CSV file for each calendar
write.csv(assign(paste("calendar_CN_", n, sep = ""), add_ID_df), file = paste("calendar_CN_", n, "_test", ".csv", sep = ""), row.names = FALSE)

}   # End loop through yearly calendars


