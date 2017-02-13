


# Read in CN calendar csv file




# Extract (into dataframe) the following columns Web.link	Start.date	End.date	event.name	event.ID


# Go to each Web.link and extract the race web links for each event.ID. Create race.ID
# At present I think the easiest thing to do will be to assign a sequential number for the race.ID
# So we'll combine the event.ID with a sequential number to create the race.ID
# Create a dataframe for each event with columns for the race weblink, date, start.location, finish.location
# Currently script for this is in 170207 cyclingnews race webscrape v1.R



# Open each race weblink and extract tables
# Currently this is acheived with the function getCNresults in the following file
# 170118 cyclingnews webscrape v1.R



# Create a dataframe for each race result table. Name it for the race.ID



test_urls <- read.csv("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files/calendar_CN_2009FeniouxFranceTrophy.csv", header = TRUE, sep = ",")

url_race1 <- paste("http://www.cyclingnews.com/", as.character(test_urls[1,1]), sep = "")
url_race2 <- paste("http://www.cyclingnews.com/", as.character(test_urls[2,1]), sep = "")

race.ID <- as.character(test_urls[1,2])


tables_out <- getCNresults(url_race1, race.ID)
tables_out

my_url <- url_race1


my_df <- iris

colnames(my_df)[3]

my_df[1,3]

