# Quick bit of code using rvest to extract the calendar
# data from Cycling News.
# I think I may have gone with a more laborious loop method
# in order to capture the race_weblink data, that isn't on 
# every line of the tables.

require(rvest)
require(lubridate)
my_url <- "http://www.cyclingnews.com/races/calendar/2011/"
my_html <- read_html(my_url)

cn_nodes <- html_nodes(my_html, "table")
cn_calendar <- c()
for(i in 1:length(cn_nodes)){
  # extract the table data for each month
  cn_table <- html_table(cn_nodes)[[i]]
  # Combine the latest month table into the overall year table
  cn_calendar <- rbind(cn_calendar, cn_table)
}
# Add the year so this can be filtered in the master calendar table
cn_calendar$year <- 2011   
View(cn_calendar)

# This doesn't do what I wanted it to.
# It splits the first two items. 'Sep' does the same thing it does in the 'paste' function
# cn_calendar <- tidyr::separate(cn_calendar, Date, c("start_date", "end_date", sep = " to ", remove = TRUE))
