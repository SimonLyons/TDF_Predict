# Script writes function to extract race results tables from CN results web links.
#
#
# load relevant libraries
# library(xml2)     # library for downloading and working with XML
# library(rvest)   # library for working with HTML nodes
# library(RCurl)
# library(magrittr)   # Library to use the package 'piping' action - %>%
# library(HTML)
library(XML)


# build function to take results url from Cycling News and return data.frame table
getCNresults <- function(my_url){
  
# Set directory to write results datafiles
  setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")
  
#  my_url_file <- "my_url_file.xml"    # not necessary to create local xml file
#  download.file(my_url, destfile = my_url_file, method = 'auto')
  my_url_parse <- htmlParse(my_url)
  table_no <- length(readHTMLTable(my_url_parse))   #   determine number of tables on url
  table_list <- c()
  
  # Use FOR LOOP for the number of tables 'i'
    for (i in 1:table_no){
      my_results <- as.data.frame(readHTMLTable(my_url_parse)[i])   # extract table number i
      
      # Extract table header information
      table_header_text <- xpathSApply(my_url_parse, "//h4", xmlValue)[1]   # There are two h4 elements. We only want the first.
      table_caption_text <- xpathSApply(my_url_parse, "//caption", xmlValue)
      table_caption_text[1] <- table_header_text   # fill the first blank result with the h4 header text
      length(table_caption_text)
      
      # Create data frame table with correct Title and column names
      header_df <- my_results[1,1:3]
      colnames(my_results)[1] <- toString(my_results[1,1])
      colnames(my_results)[2] <- toString(my_results[1,2])
      colnames(my_results)[3] <- toString(my_results[1,3])
      # attr(table_13, which = "title") <- table_caption_text[i]   # Not sure how to assign title to table yet
      my_results <- my_results[-1,]   # delete first row which contains column names
      my_results <- my_results[,-4]   # delete fourth column which is empty
      assign(paste("table_", i, sep = ""), my_results)   # Create unique table with 'i' appended
      write.csv(my_results, file = paste("table_", i, ".csv", sep = ""), row.names = FALSE)
      table_list <- c(table_list, paste("table_", i, sep = ""))
      
    }   # end FOR LOOP for number of tables
  
  return(my_results)
  # return(table_1)   # Need to work out how to return all of the tables created in the loop
}
