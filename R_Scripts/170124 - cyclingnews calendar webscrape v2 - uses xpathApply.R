# Script to attempt to extract relevant elements from Cycling News calendar webite.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

library(XML)

# Need to create the loop that cycles through all of the relevant years
my_url <-  "http://www.cyclingnews.com/races/calendar/2016"
my_url_parse <- htmlParse(my_url)

# Divide the xml data into row elements, divided by /tr node with 'data-i' inluded
td_ns <- xpathApply(my_url_parse, "//tr[@data-i]")

# Create blank calendar table
calendar_cn <- as.data.frame(matrix(NA, nrow = 0, ncol = 6))
colnames(calendar_cn)[1] <- "Date"
colnames(calendar_cn)[2] <- "Race.Details"
colnames(calendar_cn)[3] <- "Discipline"
colnames(calendar_cn)[4] <- "Location"
colnames(calendar_cn)[5] <- "UCI.code"
colnames(calendar_cn)[6] <- "Web.link"


# FOR loop to run through the number of row entries in the calendar table
for(n in 1:length(td_ns)){
  my_r1 <- td_ns[[n]]
  
  # FOR loop to run through each column (for each row) and input the relevant XML attribute
  for(i in 1:5){
    calendar_cn[n, i] <- (xmlValue(my_r1[[i]]))
    
  } # End 'i' loop to run over calendar columns
  
  # IF statement to check for existence of web link
  if(!is.null(xmlAttrs(my_r1[[2]][[1]]))){
    calendar_cn[n, 6] <- paste("http://www.cyclingnews.com", xmlAttrs(my_r1[[2]][[1]]), sep = "")
   
  }   # End if statement
} # End 'n' loop to run over calendar rows
  
# Remove "\t" and "\n" from Date column and Race.Details column
calendar_cn$Date <- gsub("\t", "", calendar_cn$Date)
calendar_cn$Date <- gsub("\n", "", calendar_cn$Date)
calendar_cn$Race.Details <- gsub("\t", "", calendar_cn$Race.Details)
calendar_cn$Race.Details <- gsub("\n", "", calendar_cn$Race.Details)

