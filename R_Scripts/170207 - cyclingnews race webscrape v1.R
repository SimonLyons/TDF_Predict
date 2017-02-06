# Script to extract relevant elements from Cycling News individual race webite.
# This is the website link I extracted from the calendar page.
# This method uses xpathApply to look at the nodes and divide it into relevant rows

library(XML)

# set working directory
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files")

# Read in relevant weblink from the calendar .csv file.
calendar_2016 <- read.csv("calendar_CN_2016.csv", header = TRUE, sep = ",")

# Extract relevant weblink for first race (Bpost bank trofee - GP Sven Nys)
race_url <- calendar_2016$Web.link[1]

race_xml <- htmlParse(race_url)

# elite_men_link <- xpathApply(race_xml, '//li/a[contains(@href, "/bpost-bank-trofee-gp-sven-nys-2016/")]', xmlAttrs)

# This line does a good job of isolating the XML attributes containing race web link info
elite_men_link <- xpathApply(race_xml, '//li/a[contains(@href, "/results")]', xmlAttrs)



result_link <- elite_men_link[grep("results", elite_men_link)][2]

results_xml <- htmlParse(paste("http://www.cyclingnews.com/races/calendar/",result_link, sep = "" ))

text()[contains("results")]