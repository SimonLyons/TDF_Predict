


test_urls <- read.csv("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files/calendar_CN_TourdeRomandie.csv", header = TRUE, sep = ",")

url_race1 <- paste("http://www.cyclingnews.com/", as.character(test_urls[1,1]), sep = "")
url_race2 <- paste("http://www.cyclingnews.com/", as.character(test_urls[2,1]), sep = "")

race.ID <- "TdR16P"


tables_out <- getCNresults(url_race1, race.ID)
tables_out
