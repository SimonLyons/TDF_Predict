# Script to extract relevant elements from Cycling News individual race webite.
# This is the website link I extracted from the calendar page.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

GetRaceWebLinks <- function(input_year){
  
# Load necessary packages
# require(XML)   # I will ideally extract all 'XML' functions and replace with 'rvest'
require(RMySQL)
require(rvest)
require(lubridate)
require(dplyr)
  
# Set working directory to user passwords location (Study PC)
# setwd("C:/b_Data_Analysis/Database")
# Set working directory for HP Laptop
# setwd("/home/data_analysis/database")
setwd("/home/a_friend/data_analysis/database/")

# Read password file (local connection)
psswd <- read.csv("passwords_db.csv", header = TRUE)
# conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]),
                        # password = as.character(psswd[psswd$type == "Manager", "password"]),
                        # dbname='ProCycling', host='localhost')  

# Read password file (remote connection)
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='192.168.1.5', port=3306) 

# Read in calendar dataframe from the ProCycling database
calendar_CN <- dbGetQuery(conn_local, paste("SELECT * FROM race_calendar_", input_year, " ;", sep = ""))

# Create a counter that keeps a running tally of the columns in the master dataframe
# Starts at 1 (obviously) and then adds a row with each new race weblink
col_counter <- 1

# Create an empty master dataframe
races_master <- as.data.frame(matrix(data = NA, 0, ncol = 8 ))
colnames(races_master) <- c("Stage", "date", "location", "distance", "stage_url", "stage_id", "race_id", "race_details")

# Use Text Progress Bar
total <- nrow(calendar_CN)
# Create text progress bar
prg <- txtProgressBar(min = 0, max = total, style = 3)

# FOR loop to run through all of the events listed in the calendar dataframe
for(e in 1:total){
  # Setup text-based progress bar
  setTxtProgressBar(prg, e)
  
  # Set race_cn to empty for each loop iteration
  races_cn <- NA

  # The LOOP needs to ignore events with no weblink
  if ((!is.na(calendar_CN$web_link[e])) && (nchar(calendar_CN$web_link[e]) > 7)){
    # Extract relevant weblink and race name
    race_url <- paste("http://www.cyclingnews.com", calendar_CN$web_link[e], sep = "") 
    race_details <- calendar_CN$race_details[e]
    race_id <- calendar_CN$race_id[e]

    # Download race data using 'rvest'
    if(RCurl::url.exists(race_url)){
    suppressWarnings(download.file(race_url, "race_url.xml", quiet = TRUE))
    race_html <- read_html("race_url.xml")}
    
    
    #######################################################################
    # In some circumstances, the stages table is located on another webpage, 
    # accessed via a link
    
    # First go and extract potential link to stage table webpage
    race_stages_link <- race_html %>% 
      html_nodes(xpath="//a[contains(@href, '/stages')]") %>% 
      html_attr("href")
    
    # If this link exists, we will replace the current webpage html
    # with the html from the stage web link
    if(length(race_stages_link) > 0){
      race_url <- paste("http://www.cyclingnews.com", race_stages_link, sep = "")
      
      if(RCurl::url.exists(race_url)){
        suppressWarnings(download.file(race_url, "race_url.xml", quiet = TRUE))
        race_html <- read_html("race_url.xml")
        }   # End IF statement checking for website errors
      
    }   # End IF statement checking for link to race stages
    #######################################################################
    
   
    # Extract the stage link information using rvest functions
    race_links <- race_html %>% 
      html_nodes(xpath="//a[contains(@href, '/results')]") %>% 
      html_attr("href")
    
    # Now remove the duplicates. The length should now match the correct number of stages.
    race_links <- race_links[!duplicated(race_links)]
    
    # Insert try() function. Possibly consider use of tryCatch() function once
    # I'm confident in its use.
    # try(   # I've inserted this to allow the weblinks function to progress past errors
    
    # Set the working directory to the location of the error log CSV file (Study PC)
    # setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files/")
    # Set the working directory to the location of the error log CSV file (HP Laptop)
    setwd("/home/a_friend/data_analysis/projects/TDF_Predict/")
    
    #######################################################################
    # The following script is used to create a new weblinks error list csv file
    # It's not required after the computer being used has a file
    # 
    error_list <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 6))
    colnames(error_list) <- c("Year", "Row_No", "Error", "Error_Counter", "Fork_No", "race_url")
    write.csv(error_list, "weblinks_error_list.csv", row.names = FALSE)
    # 
    #######################################################################
    
    #######################################################################
    #
    # There are two forks here. One for dates (and stage data) stored in a table
    # and one for dates and stage information stored in a more relaxed divided format
    # 
    #######################################################################
    
    #######################################################################
    # 
    # Fork one: Table date format
    # 
    # Do a test for the 'table' node to see if a table exists.
    # If it does, use the table method to extract the stage dates and other data.
    tryCatch({if(length(html_nodes(race_html, xpath="//table")) > 0){
      # Extract information when stored in table
      races_cn <- race_html %>% 
        html_nodes(xpath="//table") %>% 
        html_table(fill = TRUE, trim = TRUE) %>% 
        as.data.frame()
      
      # Next check for correct table type by looking for tables
      # containing the 'results' column
      # If true, go and extract the table and perform formatting
      
      if("results" %in% colnames(races_cn)){
        # At the moment the table has additional rows
        # Delete rest day rows, where 'results' = ""
        races_cn <- races_cn %>% 
          filter(results != "")
        
        # Delete duplicate rows. This occured in the 2012 TDU
        races_cn <- races_cn[!duplicated(races_cn),]

        if(length(races_cn$Stage) > 0){
          # Add race_links to table
          if(length(race_links) >0){
            
            # Need to deal with the scenario where there are less race weblinks
            # than stages in the table. There are some dodgy tables.
            # example:   http://www.cyclingnews.com/races/czech-cyclo-cross-championships-cn/stages/
            
            if(length(race_links) < length(races_cn$Stage)){
              race_links <- rep_len(race_links, length(races_cn$Stage))
              # The above isn't a fantastic result, but it deals with the odd dodgy table
            }   # End IF statement looking for number of race weblinks lower than number of stages listed in table.
            
            races_cn$stage_url <- race_links
  
          }   # End IF statement that seeks to add race_links to table
          
          # Convert date values to correct class using lubridate
          races_cn$date <- mdy(races_cn$date)
          # Convert distance values to correct 'numeric' class
          races_cn$distance <- as.numeric(gsub(" km", "", races_cn$distance))
          # Remove unecessary columns
          if(length(races_cn$stage_url > 0)){
            races_cn <- races_cn %>% select(Stage, date, location, distance, stage_url)
          }
          
          # Add my data into the dataframe
          for(n in 1:length(races_cn$Stage)){
            races_cn$stage_id[n] <- paste(race_id, "_s", formatC(n, width = 2, format = "d", flag = "0"), sep = "")
            races_cn$race_id <- as.character(race_id)
            races_cn$race_details <- as.character(race_details)
        }   # End FOR loop (n) for addition of my data.
          
        }   # IF Statement confirming there is data in the table (Stages)
        
      }   # End IF statement checking for results column in the column names

      # End IF statement and start WARNING and ERROR component of tryCatch() function
    }
      }, warning = function(war) {   # Ends IF statement to check for table (FORK ONE) and extract data
      print(paste0("Calendar Year ", input_year, ", row ", e, ", WARNING: ", war))
      error_list <- read.csv(file = "weblinks_error_list.csv")
      error_counter <- nrow(error_list) + 1
      error_list[error_counter, 1] <- input_year
      error_list[error_counter, 2] <- e
      error_list[error_counter, 3] <- war
      error_list[error_counter, 4] <- error_counter
      error_list[error_counter, 5] <- 2
      error_list[error_counter, 6] <- race_url
      write.csv(error_list, "weblinks_error_list.csv", row.names = FALSE)
      
    }, error = function(err) {
      print(paste0("Calendar Year ", input_year, ", row ", e, ", ERROR: ", err))
      error_list <- read.csv(file = "weblinks_error_list.csv")
      error_counter <- nrow(error_list) + 1
      error_list[error_counter, 1] <- input_year
      error_list[error_counter, 2] <- e
      error_list[error_counter, 3] <- err
      error_list[error_counter, 4] <- error_counter
      error_list[error_counter, 5] <- 2
      error_list[error_counter, 6] <- race_url
      write.csv(error_list, "weblinks_error_list.csv", row.names = FALSE)
    
    }   # End 'error' section
    )   # End tryCatch() function

    #######################################################################
    # 
    # Fork two: Non-table date format
    #     
    
    # Extract the stage dates. Convert to correct date format using lubridate.
    tryCatch({if(!(length(html_nodes(race_html, xpath="//table")) > 0) | !("date" %in% colnames(races_cn))){
      stage_dates <- race_html %>% 
        html_nodes(xpath="//time[@class='datetime small']") %>% 
        html_text() %>% 
        mdy_hm(truncated = 3) %>%    # Coverts string format dates into date format using Lubridate
        as_date()
      # Sometimes there is a race_link, but no date. So we'll take the date from the calendar:
      if(!(length(stage_dates)>0)){stage_dates <- calendar_CN$start_date[e]}
      
      # IF statement to only perform extraction if there are results links in the html data
      if (length(race_links) > 0){
        # create single column dataframe to capture webpage links to races
        races_cn <- as.data.frame(matrix(data = NA, nrow = length(race_links), ncol = 8 ))
        colnames(races_cn) <- c("Stage", "date", "location", "distance", "stage_url", "stage_id", "race_id", "race_details")
        class(races_cn$date) <- "Date"
        # colnames(races_cn)[1] <- "stage_url"
        # colnames(races_cn)[2] <- "stage_id"
        # colnames(races_cn)[3] <- "race_id"
        # colnames(races_cn)[4] <- "race_details"
        # colnames(races_cn)[5] <- "stage_date"
        
        # Run LOOP to pull out the weblink to the results page
        # This actually took me ages to isolate just the 'href' value
        # Currently has an error thanks to the stage date information being a shorter
        # string that the stage links information. (Duplicate stage links data)
        for(n in 1:length(race_links)){
          races_cn$date[n] <- ymd(stage_dates[n])
          races_cn$stage_url[n] <- race_links[[n]]
          races_cn$stage_id[n] <- paste(race_id, "_s", formatC(n, width = 2, format = "d", flag = "0"), sep = "")
          races_cn$race_id[n] <- as.character(race_id)
          races_cn$race_details[n] <- as.character(race_details)
          
        }   # End FOR loop (n) that pulls out weblink data
      
      }   #  END IF statement relating to whether race links exist in the HTML data
      
      # End IF statement (FORK TWO) and start WARNING and ERROR component of tryCatch() function
    }
      }, warning = function(war) {   # Ends IF statement (FORK TWO) to check for table (FORK ONE) and extract data
      print(paste("Calendar Year ", input_year, ", row ", e, ", WARNING: ", war))
      error_list <- read.csv(file = "weblinks_error_list.csv")
      error_counter <- nrow(error_list) + 1
      error_list[error_counter, 1] <- input_year
      error_list[error_counter, 2] <- e
      error_list[error_counter, 3] <- war
      error_list[error_counter, 4] <- error_counter
      error_list[error_counter, 5] <- 2
      error_list[error_counter, 6] <- race_url
      write.csv(error_list, "weblinks_error_list.csv", row.names = FALSE)
      
    }, error = function(err) {
      print(paste("Calendar Year ", input_year, ", row ", e, ", ERROR: ", err))
      error_list <- read.csv(file = "weblinks_error_list.csv")
      error_counter <- nrow(error_list) + 1
      error_list[error_counter, 1] <- input_year
      error_list[error_counter, 2] <- e
      error_list[error_counter, 3] <- err
      error_list[error_counter, 4] <- error_counter
      error_list[error_counter, 5] <- 2
      error_list[error_counter, 6] <- race_url
      write.csv(error_list, "weblinks_error_list.csv", row.names = FALSE)
      
    })   # End tryCatch() function
    
    # Row bind the small races_cn dataframe to the races_master dataframe
    # Unfortunately I've got two IF statements
    # The first checks that I've actually got a dataframe with a column for 'Stage' and
    # the second checks to see if the dataframe is empty or not.
    if("Stage" %in% colnames(races_cn)){
      if(length(races_cn$Stage) > 0){
        # Clean out special characters from the newly scraped stage location data
        races_cn$location <- text_clean(races_cn$location)
        
        # I haven't done it yet - but it seems apparent I probably should have 
        # set up a text clean or the Stage details as well.
        # I discovered this whilst attempting to build a master race weblinks table
        
        # Set location from Calendar dataframe if scraped location is empty
        races_cn[is.na(races_cn$location), "location"] <- calendar_CN$location[e]
        
        # We'll use tryCatch() to deal with issues binding tables
        tryCatch({
          races_master <- rbind(races_master, races_cn)
        }, warning = function(war) {   # Ends IF statement (FORK TWO) to check for table (FORK ONE) and extract data
          print(paste("Calendar Year ", input_year, ", row ", e, ", WARNING: ", war))
          error_list <- read.csv(file = "weblinks_error_list.csv")
          error_counter <- nrow(error_list) + 1
          error_list[error_counter, 1] <- input_year
          error_list[error_counter, 2] <- e
          error_list[error_counter, 3] <- war
          error_list[error_counter, 4] <- error_counter
          error_list[error_counter, 5] <- 3
          error_list[error_counter, 6] <- race_url
          write.csv(error_list, "weblinks_error_list.csv", row.names = FALSE)
          
        }, error = function(err) {
          print(paste("Calendar Year ", input_year, ", row ", e, ", ERROR: ", err))
          error_list <- read.csv(file = "weblinks_error_list.csv")
          error_counter <- nrow(error_list) + 1
          error_list[error_counter, 1] <- input_year
          error_list[error_counter, 2] <- e
          error_list[error_counter, 3] <- err
          error_list[error_counter, 4] <- error_counter
          error_list[error_counter, 5] <- 3
          error_list[error_counter, 6] <- race_url
          write.csv(error_list, "weblinks_error_list.csv", row.names = FALSE)
        })   # End tryCatch() for rbind
        
      }   # End IF statement checking if dataframe is empty
    }   # End IF statement looking for a complete table (with column 'Stage') ready for binding
    
    # )   # End TRY function
    
    
    }   # End IF statement identifying whether a race result link exists for each calendar entry
    
  # Insert sleep script to randomise a system pause between web queries
  sleep <- abs(rnorm(1)) + runif(1, 0, .25)
  message("I have done ", e, " of ", total,
          " - gonna sleep ", round(sleep, 2),
          " seconds.")
  Sys.sleep(sleep)
  
}   #   End FOR loop 'e' to run through all of the events in the calendar dataframe

close(prg)   # End txt progress bar

# Write 'race_master' dataframe to ProCycling database
dbWriteTable(conn_local,type = 'UTF-8', name = paste("race_weblinks_", input_year, sep = ""), races_master,
             overwrite = TRUE, row.names = FALSE)
return(races_master)

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

}   # End function GetRaceWebLinks
