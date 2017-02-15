
# Function write_race_results_tables

# Build function to take results url from Cycling News and save .csv files with each race result. 
# Input variables are the race results URL and the unique race.ID code.
# This is my unique race code, to be used in the database as a KEY identifier.
write_race_results_tables <- function(my_url, race.ID){
  
# Set directory to write results datafiles
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

require(XML)    
  my_url_parse <- htmlParse(my_url)
  table_no <- length(readHTMLTable(my_url_parse))   #   determine number of tables on url
  table_list <- c()   # Create empty table list for use in loop
  
  # Use FOR LOOP for the number of tables 'i'
    for (i in 1:table_no){
      my_results <- as.data.frame(readHTMLTable(my_url_parse)[i])   # extract table number i
      
      # Extract table header information
      table_header_text <- xpathSApply(my_url_parse, "//h4", xmlValue)[1]   # There are two h4 elements. We only want the first.
      table_caption_text <- xpathSApply(my_url_parse, "//table/caption", xmlValue)
      # My attempt to match captions to tables
      # Some CN tables have no caption - the title of the table is the 'h4' title preceding the table
      # Other CN results tables have a caption matching each table (which should be used as the table title)
      if (length(table_caption_text) == table_no){   
        table_titles <- table_caption_text
      } else{
      
      table_titles <- c(table_header_text, table_caption_text)
      }
      
      # Create data frame table with correct Title and column names
      # header_df <- my_results[1,1:3]
      if (ncol(my_results) > 0){   # IF statement required to eliminate empty tables
      for(h in 1: ncol(my_results)){
      colnames(my_results[h]) <- toString(my_results[1, h])
      }   # End 
      # attr(table_13, which = "title") <- table_caption_text[i]   # Not sure how to assign title to table yet
      my_results <- my_results[-1,]   # delete first row which contains column names
      # my_results <- my_results[,-4]   # delete fourth column which is empty
      assign(paste("table_", i, sep = ""), my_results)   # Create unique table with 'i' appended
      write.csv(my_results, file = paste(race.ID, "_TableNo_", i, "_TableCaption_", table_titles[i], ".csv", sep = ""), row.names = FALSE)
      table_list <- c(table_list, paste(race.ID, "_TableNo_", i, "_TableCaption_", table_titles[i], ".csv", sep = ""))
      }   # end IF statement
    }   # end FOR LOOP for number of tables
  
  return(table_list)
  # return(table_1)   # Need to work out how to return all of the tables created in the loop
}
