# R script to build a list of the relevant pro cycling races for a given year based on
# the Cycling News website. Aim is to build a list of the associated results web pages.
#
# load relevant libraries
library(XML)

# set working directory
setwd("C:/b Data Analysis/Sandpit/TDF_Predict")

# Create FOR loop that creates a web address for Cycling News calendars between 2005 and 2016
for (n in 2015:2016){
  my_url <- paste("http://www.cyclingnews.com/races/calendar/", n, sep = "")
  my_url_parse <- htmlParse(my_url)
  table_no <- length(readHTMLTable(my_url_parse))   #   determine number of tables on url

  # create blank calendar_cn table for each annual loop. Give it nice, neat column names
  calendar_cn <- as.data.frame(matrix(NA, nrow = 0, ncol = 5))
  colnames(calendar_cn)[1] <- "Date"
  colnames(calendar_cn)[2] <- "Race.Details"
  colnames(calendar_cn)[3] <- "Discipline"
  colnames(calendar_cn)[4] <- "Location"
  colnames(calendar_cn)[5] <- "UCI.code"  

    # For loop to read and combine each monthly table
    for (i in 1:table_no){
      # read in each monthly table and bind it to the blank header table
      my_table <- as.data.frame(readHTMLTable(my_url_parse, skip.rows = 1)[i])  
      # my_table <- my_table[-1,]   # delete first row which just has header informatoin
      colnames(my_table) <- c("Date", "Race.Details", "Discipline", "Location", "UCI.code" )
      assign(paste("table_", i, sep = ""), my_table)
      calendar_cn <- rbind(calendar_cn, assign(paste("table_", i, sep = ""), my_table))
    }   # End FOR loop to read and combine each monthly table

    # Perform actions to clean up table and insert useful columns: "Race.web", "Start", "Finish", "Stage.race"
    # Remove "\t" and "\n" from Date column
    calendar_cn$Date <- gsub("\t", "", calendar_cn$Date)
    calendar_cn$Date <- gsub("\n", "", calendar_cn$Date)
    assign(paste("calendar_CN_", n, sep = ""), calendar_cn)
    # Insert useful columns
    # assign(paste("calendar_CN_", n, "$Web.name", sep = ""), my_url)
    # assign(paste("calendar_CN_", "2016", "$Web.name", sep = ""), my_url)

}   # End loop for calendars between 2005 and 2016


# Testing subsetting on calendar table
# calendar_CN_2015[calendar_CN_2015$UCI.code == "WT", ]
# calendar_CN_2015[grep("April", calendar_CN_2015$Date), ]


# Testing extracting weblinks for each race

race_links <- xpathSApply(doc = my_url_parse, "//td/a", xmlAttrs, "href")   # Extract basic link table from html info
results_link <- paste("http://www.cyclingnews.com", race_links, "/results", sep = "")
results_link[23]

# ----------
# Using xpathApply to extract table info so that we can get both the table data and the hyperlink info

race_data_01 <- xpathApply(doc = my_url_parse, "//td/a", xmlValue & xmlAttrs)   # 
race_data_03 <- xpathApply(doc = my_url_parse, "//table/tr/td", xmlValue)   # 
table(xpathApply(doc = my_url_parse, "//table/tr/td", xmlValue))[3]
head(race_data_03)

# ----------

# Test getting the html data from a results link
race_parse <- htmlParse(paste("http://www.cyclingnews.com", gsub("href", "", race_links)[23], "/results", sep = ""))

# Test binding the Race.Details and results links into a data frame
new_df <- cbind(as.character(calendar_cn$Race.Details[1:50]), paste("http://www.cyclingnews.com", gsub("href", "", race_links), "/results", sep = "")[1:50])

# Experimenting with ways to extract web links directly from table when the table is read in. 
# Looking for ways to match up the rows efficiently
new_table <- readHTMLTable(my_url_parse, which = 4:5, skip.rows = 1)
new_table

ns <- getNodeSet(my_url_parse, "//td")[1:10]
xvns <- xmlValue(ns[[3]])
xnts <- xmlAttrs(ns[[2]])