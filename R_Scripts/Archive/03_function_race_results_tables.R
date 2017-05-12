
# Function write_race_results_tables

# Build function to take results url from Cycling News and save database tables with each race result. 
# Input variables are the race results URL and the unique race_id code.
# This is my unique race code, to be used in the database as a KEY identifier.
write_race_results_tables <- function(my_url, race_id){
  
require(RMySQL)    # For database functions
require(XML)       # For webscraping functions
  
# Set working directory to user passwords location
setwd("C:/b_Data_Analysis/Database")
# Read database password file
psswd <- read.csv("passwords_db.csv", header = TRUE)

# Create connection to database 
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='localhost') 

  my_url <- paste("http://www.cyclingnews.com", my_url, sep = "")
  my_url_parse <- htmlParse(my_url)
  table_no <- length(readHTMLTable(my_url_parse))   #   determine number of tables on url
  table_list <- c()   # Create empty table list for use in loop
  
  # Extract table column titles - need to check for tables with no column titles
  table_column_text <- xpathApply(my_url_parse, "//table/tbody/tr[@class = 'headers']/th", xmlValue)
  # Extract table header/caption information
  table_header_text <- xpathApply(my_url_parse, "//h4", xmlValue)[1]   # There are two h4 elements. We only want the first.
  table_caption_text <- xpathApply(my_url_parse, "//table/caption", xmlValue)

  # Use FOR LOOP for the number of tables 'i'
    for (i in 1:table_no){
      my_results <- as.data.frame(readHTMLTable(my_url_parse)[i])   # extract table number i
      # Some CN tables have no caption - the title of the table is the 'h4' title preceding the table
      # Other CN results tables have a caption matching each table (which should be used as the table title)
      if (length(table_caption_text) == table_no){   
        table_titles <- table_caption_text
      } else{
      # If the first table doesn't have a caption, then assign the first table title to the 'h4' title
      table_titles <- c(table_header_text, table_caption_text)
      }
      
      # Create data frame table with correct Title and column names
      if (ncol(my_results) > 0){   # IF statement required to eliminate empty tables
      
        # Insert IF statement to allow for the existence of column title information
        # If there are no column titles, then this step will be skipped
        if(length(table_column_text) > 0){
          
          for(h in 1: ncol(my_results)){
          colnames(my_results)[h] <- as.character(my_results[1, h])
          }   # End 
          # We only delete the first row if there are column titles in the tables
          my_results <- my_results[-1,]   # delete first row which contains column names
          # Replace characters that can't be used in column names in MySQL
          # Should be limited to alpha, numeric and '_'
          colnames(my_results) <- gsub("#", "pos", colnames(my_results))
          colnames(my_results) <- gsub(" ", "", colnames(my_results))
          colnames(my_results) <- gsub("\\(|)", "_", colnames(my_results))
        }
      
      # Need to find a way to delete empty columns. Not a priority right now (15FEB17)
      # my_results <- my_results[,-4]   # delete fourth column which is empty
      
        # Function to strip out UTF-8 characters from the table
        my_results$RiderName_Country_Team <- text_clean(my_results$RiderName_Country_Team)

        # Write the table to its location (now to the MySQL database)
        dbWriteTable(conn_local, name = paste(race_id, "_t", 
                                        formatC(i, width = 2, format = "d", flag = "0"), sep = ""), 
                                        my_results, overwrite = TRUE, row.names = FALSE)
        table_list <- c(table_list, paste(race_id, "_t", formatC(i, width = 2, format = "d", flag = "0"), "_", fsSafe(table_titles[i]), sep = ""))
        # 
        # Close open queries (doesn't close the connection - very useful)
        ###################  NOT SURE WHY THIS IS CAUSING AN ERROR AT THE MOMENT ##############
        dbClearResult(dbListResults(conn_local)[[1]])

      }   # end IF statement for empty tables
    }   # end FOR LOOP for number of tables
  
  return(table_list)
  # return(table_1)   # Need to work out how to return all of the tables created in the loop
  
  # Script for closing all active connections to MySQL databases.
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) 
    dbDisconnect(con)
}
