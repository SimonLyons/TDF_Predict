##################################
# Significantly updated version of the function to scrape race results tables
# 
# Function write_race_results_tables

# Build function to take results url from Cycling News and save database tables with each race result. 
# Input variables are the race results URL, unique stage_id code and the stage date.
# This is my unique stage code, to be used in the database as a KEY identifier.
write_race_results_tables <- function(my_url, stage_id, stage_date){
  
require(RMySQL)    # For database functions
require(rvest)     # For webscraping
require(tidyr)     # For tidying up and wrangling scraped dataframes - I use the 'separate' function
require(lubridate) # For assigning time/date properties
require(dplyr)     # For manipulating and filtering tables
  
  # Clear database connections
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) 
    dbDisconnect(con)

##################################
# 1. Download and extract tables
##################################

# Check for NA my_url values
# Abort rest of function if there is.
if(!is.na(my_url)){
  
  # Check to see if link is expected partial web link
  if(!agrepl("http", my_url) | !agrepl("www.", my_url) | is.na(my_url)){
    my_url <- my_url <- paste("http://www.cyclingnews.com", my_url, sep = "")
  }
  # Check to see if URL exists
  if(RCurl::url.exists(my_url)){    # This IF statement runs almost to the end
    
    # try(download.file(my_url, "my_html.xml", quiet = TRUE))
    suppressWarnings(download.file(my_url, "my_html.xml", quiet = TRUE))
    my_html <- read_html("my_html.xml")
    
    
    # Read the result tables from the HTML
    # This has been modified/refined to only select 'table' nodes
    # containing the 'tbody' node. On some webpages there has been empty
    # tables which cause problems further down.
    my_table <- "my_html.xml" %>% 
      read_html() %>% 
      html_nodes(xpath="//table[.//tbody/tr]") %>% 
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
    # I've made an adjustment (06JUN17) to the xpath selector to refine the list
    # of header nodes to only those containing the text 'results'
    # Further adjustment (26JUN17) to account for case insensitive search using 'translate'
    # https://stackoverflow.com/questions/8474031/case-insensitive-xpath-contains-possible
    first_header <- "my_html.xml" %>% 
      read_html() %>% 
      html_nodes(xpath="//h4[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'results')]") %>% 
      html_text()
    if(length(first_header) > 0){
      first_header <- first_header[[1]]
    }
    
    # On rare occasions a 'h3' node is used for the 'Results' header
    # and there is no 'h4' node header
    if(length(first_header) == 0){
      first_header <- "my_html.xml" %>% 
        read_html() %>% 
        html_nodes(xpath="//h3[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'results')]") %>% 
        html_text()
    }   # End 'h3' IF statement
    
    # Insert the first header into the captions list if the number of captions
    # doesn't match the number of tables. 
    if (length(my_captions) == table_no){   
      my_captions <- my_captions
    } else{
      # If the first table doesn't have a caption, then assign the first table title to the 'h4' title
      my_captions <- c(first_header, my_captions)
    }
    
    # On rare occasions captions have invalid UTF-8 characters
    # e.g. http://www.cyclingnews.com/races/etoile-de-bessges-2013/stage-1/results/
    my_captions <- text_clean(my_captions)
    
    # If there are no tables, the rest of the function can be skipped
    # An example is Stage 2 of the 2015 Mitchelton Bay Cycling Classic
    # http://www.cyclingnews.com/races/mitchelton-bay-cycling-classic-2015/race-2/results/
    if(table_no > 0){
      
      ##################################
      # 3. Split Rider (Country) Team column into multiple columns
      # 4. Assign columns for result_classification and result_type ('time' or 'points')
      ##################################
      
      for(t in 1:table_no){
        
        # Exit if the table is empty (nrow = 0)
        if(nrow(my_table[[t]]) > 0){
          
          # I've decided to force the column names
          # There are some odd, rare exceptions and so far I don't think it's worth
          # writing extensive code when I still want a uniform end result.
          # 
          # if(colnames(my_table[[t]][1]) == "X1"){
            colnames(my_table[[t]]) <- c("#", "Rider Name (Country) Team", "Result")
          # }
          
          # Rename the first column from '#' to 'Pos'
          my_table[[t]] <- rename(my_table[[t]], "Pos" = `#`)
          
          # We want to force the rider column to be called 'Rider Name (Country) Team'
          # despite this not being the case in rare occasions.
          # Example:   
          # http://www.cyclingnews.com/races/cycling-australia-road-national-championships-2014-2014/under-23-mens-time-trial/results/
          # 
          # loc <- agrep("Rider", colnames(my_table[[t]]))
          # colnames(my_table[[t]])[[loc]] <- "Rider Name (Country) Team"
          
          # Split the 'Rider Name (Country) Team' column using the 'separate' function from Hadley's 'tidyr' package
          my_table[[t]] <- separate(data = my_table[[t]], into = c("Rider", "Remaining"), 
                                    sep = " \\(", col = "Rider Name (Country) Team", remove = TRUE, extra = "drop")
          my_table[[t]] <- separate(data = my_table[[t]], into = c("Country", "Team"), sep = "\\) ", col = "Remaining", remove = TRUE, extra = "drop")
          
          # Use my 'text_clean' function to remove special and non UTF-8 characters from the rider name and team name
          my_table[[t]]$Rider <- text_clean(my_table[[t]]$Rider)
          my_table[[t]]$Team <- text_clean(my_table[[t]]$Team)
          my_table[[t]]$Country <- text_clean(my_table[[t]]$Country)
          
          # Remove '(' from Country column
          my_table[[t]]$Country <- gsub("\\)", "", my_table[[t]]$Country)
          
          # Assign (an entire) column to the table title, so this can be filtered for analysis
          my_table[[t]][,"result_class"] <- my_captions[t]
          # Rename the 'NA' column with 'pts' to the name 'result_type'
          colnames(my_table[[t]])[6] <- "result_type"
          # If the column contains 'pts', fill the entire column with 'pts'. Otherwise, fill the column with 'time'.
          # I've replaced the exact matching '%in%' with the fuzzy matching of 'agrepl' 
          # Note: 'agrepl' returns a logical TRUE/FALSE. 'agrep' returns the location of the match
          my_table[[t]][ ,6] <- ifelse((agrepl("pts", my_table[[t]][1 ,6])) == 1 , "pts", "time")
          
          # Kill off the non-breaking spaces in the 'Result' column, for the points tables only
          # It's important to convert this to an integer. When left as a 'list' the table won't write
          # to the database.
          if("pts" %in% my_table[[t]][1 ,6]){
            my_table[[t]]$Result <- as.integer(lapply(my_table[[t]]$Result, function(y) gsub("[[:space:]]", NA, y)))
          }
          
          # Kill off the non-breaking spaces in the 'result_type' column, and make the output a character
          # If it's left as a 'list', the table won't write to the database
          my_table[[t]]$result_type <- as.character(lapply(my_table[[t]]$result_type, function(y) gsub("Â", "", y)))
          
          # For the 'time' based tables only, we will convert the result to a true time class
          # and create a new column with the correct rider time/duration for the race.
          if(my_table[[t]]$result_type[1] =="time"){
            
            ##################################
            # 5. Modify time-based 'Result' into correct class. lubridate::hms, lubridate::duration
            ##################################
            
            # I've renamed the 5th column to "Result" as some tables have an empty column
            # and therefore the "Result" title is missing.
            colnames(my_table[[t]])[5] <-  "Result"
            
            # Use lubridate::seconds::hms to do time class conversion
            # I previously used 'lubridate::as.duration' but didn't like working with the format
            my_table[[t]]$result_seconds <- seconds(hms(my_table[[t]]$Result))
            
            ##################################
            # 6. Create new column 'duration' with cumulative time
            ##################################
            # Change NA columns to value (numeric) '0'
            NA_rows <- is.na(my_table[[t]]$result_seconds)
            my_table[[t]]$result_seconds[NA_rows] <- 0
            
            
            # Create new 'duration' column with cumulative time
            # Set the first 'duration' row value as the first 'result_seconds' value
            my_table[[t]]$duration[1] <- as.integer(my_table[[t]]$result_seconds[1])
            if(nrow(my_table[[t]]) > 1){
              for(s in 2:nrow(my_table[[t]])){
                if(as.numeric(my_table[[t]]$result_seconds[s]) == 0){
                  my_table[[t]]$duration[s] <- as.integer(my_table[[t]]$duration[(s-1)])
                } else {
                  my_table[[t]]$duration[s] <- as.integer(my_table[[t]]$duration[1] + my_table[[t]]$result_seconds[s])
                }   # End ELSE statement
              }   # End FOR loop running from row '2' to the end of the table
            }   # End IF statement checking for table length greater than '1'
               # End FOR statement running through 'duration' column
            
            
            ##################################
            # 7. Correct non-finishing entries (e.g. DNF, DNS & DSQ).
            ##################################
            if(any(c("DSQ", "DNF", "DNS") %in% my_table[[t]]$Pos)){
              change_row <- my_table[[t]]$Pos %in% c("DSQ", "DNF", "DNS")
              my_table[[t]][change_row, "result_seconds"] <- NA
              my_table[[t]][change_row, "duration"] <- NA
            }   # End IF statement looking for non-finishers
            
          }   # End IF statement selecting 'time' based tables only (not points)
          
          # This ELSE statement converts the list of points into an integer class.
          else {
            my_table[[t]]$Result <- as.integer(my_table[[t]]$Result)
          }   # End ELSE statement
          
          # dplyr::mutate new 'stage_id' and 'stage_date' column
          my_table[[t]] <- mutate(my_table[[t]], stage_id = stage_id)
          my_table[[t]] <- mutate(my_table[[t]], stage_date = stage_date)
          
        }   # End IF statement checking for empty tables
        
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
      
      
      # Set working directory to user passwords location
      # setwd("C:/b_Data_Analysis/Database")
      setwd("/home/a_friend/data_analysis/database/")
      # Read database password file
      psswd <- read.csv("passwords_db.csv", header = TRUE)
      
      # Create connection to database 
      conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                              password = as.character(psswd[psswd$type == "Manager", "password"]),  
                              dbname='ProCycling', host='192.168.1.5', port=3306)
      
      # Write the 'times' table to the MySQL ProCyling database
      if(!is.null(time_tables)){
        dbWriteTable(conn_local, name = "master_results_time",
                     time_tables, overwrite = FALSE, row.names = FALSE, append = TRUE)
      }   # End of script writing to master time table
      
      # Write the 'points' table to the MySQL ProCyling database
      if(!is.null(points_tables)){
        dbWriteTable(conn_local, name = "master_results_points",
                     points_tables, overwrite = FALSE, row.names = FALSE, append = TRUE)
      }   # End of script writing to master points table
      
      dbDisconnect(conn_local)   # Close connection to database
      
    }   # Close IF statement checking for existence of results tables.
    
  }   # Close IF statement checking if URL exists
  
  
  # Script for closing all active connections to MySQL databases.
  # all_cons <- dbListConnections(MySQL())
  # for(con in all_cons) 
  #   dbDisconnect(con)

  }   # End IF statement checking for NA my_url values

}   # End overall FUNCTION
