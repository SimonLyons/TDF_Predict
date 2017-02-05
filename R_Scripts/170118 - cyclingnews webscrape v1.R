#
#
#
#
# load relevant libraries
library(xml2)     # library for downloading and working with XML
library(rvest)   # library for working with HTML nodes
library(RCurl)
library(magrittr)   # Library to use the package 'piping' action - %>%
library(HTML)

# set working directory
setwd("C:/b Data Analysis/Sandpit/TDF_Predict")

# Set url

uci_2016_calendar <- "http://www.cyclingnews.com/races/calendar/2016/"

tdu_2016_classic_url <- "http://www.cyclingnews.com/races/santos-tour-down-under-2016/down-under-classic-adelaide/results/"
tdu_2016_stage_1_url <- "http://www.cyclingnews.com/races/santos-tour-down-under-2016/stage-1/results/"

tdu_2016_stage_3_url <- "http://www.cyclingnews.com/races/santos-tour-down-under-2016/stage-3/results/"


# -------- Expressions for testing results webscrape function
# getCNresults(tdu_2016_stage_3_url)
# my_url <- tdu_2016_stage_3_url
# -------------------------------------------------------------

# build function to take results url from Cycling News and return data.frame table
getCNresults <- function(my_url){
  
#  my_url_file <- "my_url_file.xml"    # not necessary to create local xml file
#  download.file(my_url, destfile = my_url_file, method = 'auto')
  my_url_parse <- htmlParse(my_url)
  table_no <- length(readHTMLTable(my_url_parse))   #   determine number of tables on url
  
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
    }   # end FOR LOOP for number of tables
  

  return(table_1)   # Need to work out how to return all of the tables created in the loop
}

# -----------------------------------------------------
# Some potentially useful expressions
#
# getNodeSet(my_url_parse, ".//table", attributes"list")
# # xpathApply(tdu_2016_classic__parse, "//<tbody>")
# getNodeSet()


