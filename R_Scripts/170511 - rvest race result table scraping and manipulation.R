
# 1. Download and extract tables
# 2. Extract Table names
# 3. Split Rider (Country) Team column into multiple columns
# 4. Assign columns for result_classification and result_type ('time' or 'points')
# 5. Modify time-based 'Result' into correct class. lubridate::hms, lubridate::duration
# 6. Fill in empty time values. tidyr::fill
# 7. Add 'Duration' column. dplyr::mutate
# 8. Correct non-finishing entries (e.g. DNF, DNS, DNQ, etc).
# 9. Combine all 'time' tables and 'points' tables into two master tables.



require(rvest)
require(tidyr)
require(lubridate)
require(dplyr)

##################################
# 1. Download and extract tables
##################################

# Set the working directory
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files/")

# Set the cycling news race result url and download
my_url <- "http://www.cyclingnews.com/giro-ditalia/stage-4/results/"
download.file(my_url, "my_html.xml")
my_html <- read_html("my_html.xml")

# Read the result tables from the HTML
my_table <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//table") %>% 
  html_table(fill = TRUE, trim = TRUE)

##################################
# 2. Extract Table names
##################################

# Extract the table titles - called 'captions'
my_captions <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//table/caption") %>% 
  html_text()
# my_captions

# Check length of captions against number of tables
# length(my_table)
# length(my_captions)

# First table header - the first 'h4' node
# This is the XML method, which seems pretty quick
# table_header_text <- xpathApply(my_url_parse, "//h4", xmlValue)[1]

first_header <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//h4") %>% 
  html_text()
first_header <- first_header[[1]]


my_captions <- c(first_header, my_captions)

##################################
# 3. Split Rider (Country) Team column into multiple columns
# 4. Assign columns for result_classification and result_type ('time' or 'points')
##################################

for(t in 1:length(my_table)){
  # Rename first column from '#' to 'Pos'
  my_table[[t]] <- rename(my_table[[t]], "Pos" = `#`)
  
  # Split the 'Rider Name (Country) Team' column using the 'separate' function from Hadley's 'tidyr' package
  my_table[[t]] <- separate(data = my_table[[t]], into = c("Rider", "Remaining"), sep = " \\(", col = "Rider Name (Country) Team", remove = TRUE, extra = "drop")
  my_table[[t]] <- separate(data = my_table[[t]], into = c("Country", "Team"), sep = "\\) ", col = "Remaining", remove = TRUE, extra = "drop")
  
  my_table[[t]][,"result_class"] <- my_captions[t]
  colnames(my_table[[t]])[6] <- "result_type"
  my_table[[t]][ ,6] <- ifelse("pts" %in% my_table[[t]][1 ,6], "pts", "time")
  
  # Kill off the non-breaking spaces in the 'Result' column
  my_table[[t]]$Result <- lapply(my_table[[t]]$Result, function(y) gsub("[[:space:]]", NA, y))
  

  if(my_table[[t]]$result_type[1] =="time"){
  
  #   ##################################
  #   # 5. Modify time-based 'Result' into correct class. lubridate::hms, lubridate::duration
  #   ##################################
  #   # Use lubridate::as.duration::hms to do time class conversion
      my_table[[t]]$Result <- as.duration(hms(my_table[[t]]$Result))
     
  #   ##################################
  #   # 6. Fill in empty time values. tidyr::fill
  #   ##################################
      # Change NA columns to value (numeric) '0'
      NA_rows <- is.na(my_table[[t]]$Result)
      my_table[[t]]$Result[NA_rows] <- 0
      # dplyr::mutate new "Duration" column
      my_table[[t]] <- mutate(my_table[[t]], Duration = cumsum(Result))
      # Convert new column into correct date format
      my_table[[t]]$Duration <- duration(my_table[[t]]$Duration)
      
      ##################################
      # 8. Correct non-finishing entries (e.g. DNF, DNS & DSQ).
      ##################################
            if(any(c("DSQ", "DNF", "DNS") %in% my_table[[t]]$Pos)){
              change_row <- my_table[[t]]$Pos %in% c("DSQ", "DNF", "DNS")
              my_table[[t]][change_row, "Result"] <- NA
              my_table[[t]][change_row, "Duration"] <- NA
      }   # End IF statement looking for non-finishers
  
    }   # End IF statement selecting 'time' based tables only (not points)
  
  
}   # End FOR loop through number of tables, 't'

##################################
# 9. Combine all 'time' tables and
# 'points' tables into two master tables.
##################################

# Create holding tables/variables
time_tables <- c()   # Used to combine all the 'time' based tables
points_tables <- c()   # Used to combine all the 'points' based tables

# Combine points tables and time tables into two (2) collective holding tables
for(t in 1:(length(my_table))){
  ifelse(my_table[[t]]$result_type[1] == "time", time_tables <- rbind(time_tables, my_table[[t]]), points_tables <- rbind(points_tables, my_table[[t]]))
}





#######################################################################################
# Try rvest on Velogames

velo_url <- "https://www.velogames.com/giro-ditalia/2017/teamroster.php?tid=59092f09eec43613"
download.file(velo_url, "velo_html.xml")

# Read the column names for the table
velo_table_colnames <- "velo_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//thead/th") %>% 
  html_text() %>%
  { gsub("\\r\\n", "", .) }   # There are some messy escape characters that need removal
# The first column name is empty, which we will assign the name 'Rider'
velo_table_colnames[1] <- "Rider"

# Extract the table
velo_table <- "velo_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//table") %>% 
  html_table(fill = TRUE)
velo_table <- velo_table[[2]]

# Assign the correct column names
colnames(velo_table) <- velo_table_colnames

View(velo_table)
