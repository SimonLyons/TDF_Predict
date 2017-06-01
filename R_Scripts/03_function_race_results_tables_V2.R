##################################
# Significantly updated version of the function to scrape race results tables
# 
# Function write_race_results_tables

# Build function to take results url from Cycling News and save database tables with each race result. 
# Input variables are the race results URL and the unique stage_id code.
# This is my unique race code, to be used in the database as a KEY identifier.
write_race_results_tables <- function(my_url, stage_id, stage_date){
  
require(RMySQL)    # For database functions
require(rvest)     # For webscraping
require(tidyr)     # For tidying up and wrangling scraped dataframes - I use the 'separate' function
require(lubridate) # For assigning time/date properties
require(dplyr)     # For manipulating and filtering tables
  
# Set working directory to user passwords location
setwd("C:/b_Data_Analysis/Database")
# Read database password file
psswd <- read.csv("passwords_db.csv", header = TRUE)

# Create connection to database 
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='localhost') 

##################################
# 1. Download and extract tables
##################################

my_url <- paste("http://www.cyclingnews.com", my_url, sep = "")
suppressWarnings(download.file(my_url, "my_html.xml", quiet = TRUE))
my_html <- read_html("my_html.xml")


# Read the result tables from the HTML
my_table <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//table") %>% 
  html_table(fill = TRUE, trim = TRUE)

# Determine number of tables
table_no <- length(my_table)

##################################
# 2. Extract Table names
##################################

# Extract the table titles - called 'captions'
my_captions <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//table/caption") %>% 
  html_text()

# Read in the header artifact - often the name of the first table ('Full Results')
first_header <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//h4") %>% 
  html_text()
first_header <- first_header[[1]]

if (length(my_captions) == table_no){   
  my_captions <- my_captions
} else{
  # If the first table doesn't have a caption, then assign the first table title to the 'h4' title
  my_captions <- c(first_header, my_captions)
}

##################################
# 3. Split Rider (Country) Team column into multiple columns
# 4. Assign columns for result_classification and result_type ('time' or 'points')
##################################

for(t in 1:table_no){
  # Rename first column from '#' to 'Pos'
  my_table[[t]] <- rename(my_table[[t]], "Pos" = `#`)
  
  # Split the 'Rider Name (Country) Team' column using the 'separate' function from Hadley's 'tidyr' package
  my_table[[t]] <- separate(data = my_table[[t]], into = c("Rider", "Remaining"), sep = " \\(", col = "Rider Name (Country) Team", remove = TRUE, extra = "drop")
  my_table[[t]] <- separate(data = my_table[[t]], into = c("Country", "Team"), sep = "\\) ", col = "Remaining", remove = TRUE, extra = "drop")
  View(my_table[1])
  # Use my 'text_clean' function to remove special and non UTF-8 characters from the rider name
  my_table[[t]]$Rider <- text_clean(my_table[[t]]$Rider)
  
  # Assign (an entire) column to the table title, so this can be filtered for analysis
  my_table[[t]][,"result_class"] <- my_captions[t]
  # Rename the 'NA' column with 'pts' to the name 'result_type'
  colnames(my_table[[t]])[6] <- "result_type"
  # If the column contains 'pts', fill the entire column with 'pts'. Otherwise, fill the column with 'time'.
  # I've replaced the exact matching '%in%' with the fuzzy matching of 'agrepl' 
  # Note: 'agrepl' returns a logical TRUE/FALSE. 'agrep' returns the location of the match
  my_table[[t]][ ,6] <- ifelse((agrepl("pts", my_table[[t]][1 ,6])) == 1 , "pts", "time")
  
  # Kill off the non-breaking spaces in the 'Result' column
  
  
  # OOPS!! My fix in this next part to sort out the 'list' in points tables accidentally wiped
  # the result data when there is a time result
  # my_table[[t]]$Result <- as.integer(lapply(my_table[[t]]$Result, function(y) gsub("[[:space:]]", NA, y)))
  
  my_table[[t]]$Result <- lapply(my_table[[t]]$Result, function(y) gsub("[[:space:]]", NA, y))
  # my_table[[t]]$result_type <- lapply(my_table[[t]]$result_type, function(y) gsub("Â", "", y))
  
  
  
  # For the 'time' based tables only, we will convert the result to a true time class
  # and create a new column with the correct rider time/duration for the race.
  if(my_table[[t]]$result_type[1] =="time"){
    
    ##################################
    # 5. Modify time-based 'Result' into correct class. lubridate::hms, lubridate::duration
    ##################################
    # Use lubridate::as.duration::hms to do time class conversion
    my_table[[t]]$Result <- as.duration(hms(my_table[[t]]$Result))
    
    ##################################
    # 6. Create new column 'Duration' with cumulative time
    ##################################
    # Change NA columns to value (numeric) '0'
    NA_rows <- is.na(my_table[[t]]$Result)
    my_table[[t]]$Result[NA_rows] <- 0
    # dplyr::mutate new "Duration" column
    my_table[[t]] <- mutate(my_table[[t]], Duration = cumsum(Result))
    # Convert new column into correct date format
    my_table[[t]]$Duration <- duration(my_table[[t]]$Duration)
    
    ##################################
    # 7. Correct non-finishing entries (e.g. DNF, DNS & DSQ).
    ##################################
    if(any(c("DSQ", "DNF", "DNS") %in% my_table[[t]]$Pos)){
      change_row <- my_table[[t]]$Pos %in% c("DSQ", "DNF", "DNS")
      my_table[[t]][change_row, "Result"] <- NA
      my_table[[t]][change_row, "Duration"] <- NA
    }   # End IF statement looking for non-finishers
    
  }   # End IF statement selecting 'time' based tables only (not points)
  
  # This ELSE statement converts the list of points into an integer class.
  else {
    my_table[[t]]$Result <- as.integer(my_table[[t]]$Result)
  }   # End ELSE statement
  
  # dplyr::mutate new 'stage_id' and 'stage_date' column
  my_table[[t]] <- mutate(my_table[[t]], stage_id = stage_id)
  my_table[[t]] <- mutate(my_table[[t]], stage_date = stage_date)
  
}   # End FOR loop through number of tables, 't'

##################################
# 8. Combine all 'time' tables and
# 'points' tables into two master tables.
##################################

# Create holding tables/variables
time_tables <- c()   # Used to combine all the 'time' based tables
points_tables <- c()   # Used to combine all the 'points' based tables

# Combine points tables and time tables into two (2) collective holding tables
for(t in 1:(length(my_table))){
  ifelse(my_table[[t]]$result_type[1] == "time", time_tables <- rbind(time_tables, my_table[[t]]), points_tables <- rbind(points_tables, my_table[[t]]))
}


# View(points_tables)
# View(time_tables)
# class(time_tables)

##################################
# 9. Write two sets of tables to database
##################################

# I think I might need to use the 'dbSendQuery' function to make ammendments to an existing table

# dbSendQuery(conn_local, "")


# Write the 'times' table to the MySQL ProCyling database
dbWriteTable(conn_local, name = "test_test_master_results_time",
             time_tables, overwrite = FALSE, row.names = FALSE, append = TRUE)

# Write the 'points' table to the MySQL ProCyling database
dbWriteTable(conn_local, name = "test_test_master_results_points",
             points_tables, overwrite = FALSE, row.names = FALSE, append = TRUE)
# 
# Close open queries (doesn't close the connection - very useful)
###################  NOT SURE WHY THIS IS CAUSING AN ERROR AT THE MOMENT ##############
# dbClearResult(dbListResults(conn_local)[[1]])


# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)
}
