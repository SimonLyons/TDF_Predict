# This script aims to build predictor variables, for each 
# rider completing the 2016 TDF, with their AGE.

######################################################################
# SETUP

# Load required libraries
require(RMySQL)
require(lubridate)
require(dplyr)
require(ggplot2)
require(tidyr)
require(caret)


# Read prior file with details of riders finishing the 2016 TdF
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
# View(results_df)

# Setup database connection
dbpsswddir <- set_db_password()
setwd(dbpsswddir)
psswd <- read.csv("passwords_db.csv")

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)
# Open connection to ProCyling database
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='192.168.1.7', port=3306, timeout=3600)

######################################################################
# MySQL QUERIES
# 
# I'm going to build a query pulling rider details from the rider table
# in the ProCycling database.
# 
#          ### WHERE filter based on rider###
# Build a WHERE criteria list of all the riders who completed the 2016 TdF
WHERE_rider <- paste0("rider_name = '", results_df$Rider[1], "'")
for (r in 2:length(results_df$Rider)){
  WHERE_rider <- paste0(WHERE_rider, " OR rider_name = '", results_df$Rider[r], "'")
}

# Paste together the criteria for the MySQL query
criteria <- paste0("SELECT * FROM rider_list_master WHERE ", WHERE_rider, ";")

# Perform the MySQL query, 
rider_table <- dbGetQuery(conn_local, criteria)
View(rider_table)

# Okay - so this is the first time I've gone looking for rider names as a query from
# the ProCycling database and immediately it's obvious I've not retrieved all of the
# riders in my list. 

# The other signficant observation is the dob values have incorrect years listed. The 
# date and month seem to be correct, but the year is 2019 for all the riders I've seen.

# Convert DOB column to 'Date' class
rider_table$dob <- as.Date(rider_table$dob)
glimpse(rider_table)

# The good news (regarding dob) is I can obtain the correct DOB for each rider from
# their UCI number. I've inserted this corrected dob in my table under the field 'dob_c' 
rider_table$dob_c <- ymd(substr(rider_table$uci_id , 4, 11))

# Once the results table and rider table are merged the missing riders are obvious.
rider_dob_merge <- merge(x = results_df, y = rider_table, by.x = 'Rider', by.y = 'rider_name', all = TRUE)
View(rider_dob_merge)

# Alejandro Valverde is the first big name I can see is missing. Looking at his 
# Cycling News profile which has his name as Alejandro Valverde Belmonte:
# http://www.cyclingnews.com/riders/alejandro-valverde-belmonte/
# So I'll do a query on the database for this name to check

rider_avb <- dbGetQuery(conn_local, "SELECT * FROM rider_list_master 
                        WHERE rider_name = 'Alejandro Valverde Belmonte';")
rider_avb
# The above returns data for Alejandro appropriately confirming the name issue

# I'll now use the SOUNDEX function to see if it can be used to obtain better results
# Firstly - testing with just Alejandro Valverde

rider_avb_SOUNDEX <- dbGetQuery(conn_local, "SELECT * FROM rider_list_master 
                                WHERE SOUNDEX(rider_name) = SOUNDEX('Alejandro Valverde');")
rider_avb_SOUNDEX
# The above does not return anything for a string less than 'Alejandro Valverde Belmont'
# So I'm looking for other fuzzy string matching techniques for MySQL. There MUST be some!




# So in order to use the MATCH / AGAINST query functions it appears I need to have
# the column I'm searching indexed as a FULLTEXT column. Here I modify rider_name:
dbSendQuery(conn_local, "ALTER TABLE rider_list_master ADD FULLTEXT(rider_name);")

# I can now use the FULLTEXT search command which works successfully.
rider_avb_MATCH <- dbGetQuery(conn_local, "SELECT * FROM rider_list_master 
                              WHERE MATCH(rider_name) AGAINST ('+Ben+King' IN BOOLEAN MODE);")
rider_avb_MATCH


rider_avb_MATCH <- dbGetQuery(conn_local, "SELECT * FROM rider_list_master 
                              WHERE MATCH(rider_name) AGAINST ('+Daniel+Moreno' IN BOOLEAN MODE);")
rider_avb_MATCH

# Setup query criteria for full 2016 TdF rider list
WHERE_rider <- paste0("MATCH(rider_name) AGAINST('+", gsub(" ", "+", results_df$Rider[1]), "')")
for (r in 2:length(results_df$Rider)){
  WHERE_rider <- paste0(WHERE_rider, " OR MATCH(rider_name) AGAINST('+", gsub(" ", "+", results_df$Rider[r]), "' IN BOOLEAN MODE)")
}
View(WHERE)
length(results_df$Rider)

########## Narrow MySQL FULLTEXT search results
# I need to continue working on my fuzzy name matching.

# Paste together the criteria for the MySQL query
criteria <- paste0("SELECT * FROM rider_list_master WHERE ", WHERE_rider, ";")




rider_table_MATCH <- dbGetQuery(conn_local, criteria)
# Write resulting rider list table to local working data directory
write.csv(rider_table_MATCH, "rider_table_MATCH.csv", row.names = FALSE)
View(rider_table_MATCH)
# I'm returning 160 riders in the above query, which isn't too bad. I'm therefore
# missing 14 riders. 
unique(rider_table_MATCH$rider_name)

# Use my new ClosestMatch3 function to match up the rider names between the datasets
results_df$Rider_M <- ClosestMatch3(as.character(results_df$Rider), rider_table_MATCH$rider_name)
View(results_df[ , c("Rider", "Rider_M")])

ClosestMatch2("Daniel Moreno", rider_table_MATCH$rider_name)

rider_table_MATCH$rider_name[grep("Daniel", rider_table_MATCH$rider_name)]
order(levenshteinSim("Daniel Moreno", rider_table_MATCH$rider_name), decreasing = TRUE)




# Now use the new rider name column (Rider_M) in results_df to match/merge the rider table data
rider_dob_MATCH_merge <- merge(x = results_df, y = rider_table_MATCH, by.x = 'Rider_M', by.y = 'rider_name', all = FALSE)
View(rider_dob_MATCH_merge)

# Missing riders from MATCH / AGAINST approach
rider_dob_missing <- rider_dob_MATCH_merge[is.na(rider_dob_MATCH_merge$dob), ]
View(rider_dob_missing)
# There are 35 riders in this list, which means the merge is not picking up
# all 160 riders in my database dob MATCH dataframe.

# A TRUE/FALSE list of the rows in the merged dataframe with NA results
rider_dob_MATCH_merge.na <- is.na(rider_dob_MATCH_merge$dob)

unique(rider_table_MATCH$rider_name)
results_df$Rider


 
