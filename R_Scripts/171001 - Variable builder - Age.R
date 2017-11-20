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
require(tdfAuto)


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
                        dbname='ProCycling', host='192.168.1.6', port=3306, timeout=3600)

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

# Read the above MATCH table back into R
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
rider_table_MATCH <- read.csv("rider_table_MATCH.csv", header = TRUE)
View(rider_table_MATCH)
# I'm returning 160 riders in the above query, which isn't too bad. I'm therefore
# missing 14 riders. 
unique(rider_table_MATCH$rider_name)

# Use my new ClosestMatch3 function to match up the rider names between the datasets
results_df$Rider_M <- ClosestMatch3(as.character(results_df$Rider), as.character(rider_table_MATCH$rider_name))
View(results_df[ , c("Rider", "Rider_M")])


# Daniel Moreno (31), Bartosz Huzarski (39), Thomas De Gendt (40), Jon Izaguirre (47), Rui Costa (49),
# Luis Angel Mate (55), Mikael Cherel (57), Eduardo Sepulveda (59), Michael Valgren (77), Anthony Delaplace (90),
# PierreLuc Perichon (93), Bertjan Lindeman (94), Florian Vachon (105), Nicolas Edet (106), Armindo Fonseca (146),
# Andreas Schillinger (154), Greg Henderson (155), Christophe Laporte (157), Adrien Petit (164), 
# Daniel Mclay (170)

# Create a manual file for all the incorret matches
rider_delete <- c(31, 39, 40, 47, 49, 55, 57, 59, 77, 90, 93, 94, 105, 106, 146, 154, 155, 157, 164, 170)
results_df$Rider[-rider_delete]

# Remove the incorrect matches from the results_df dataframe
results_df <- results_df[-rider_delete, ]

# Now use the new rider name column (Rider_M) in results_df to match/merge the rider table data
rider_dob_MATCH_merge <- merge(x = results_df, y = rider_table_MATCH, by.x = 'Rider_M', by.y = 'rider_name', all = FALSE)

# Insert rider age column. I've currently got 1/07/2016 as the date used to determined rider age
# but this should strictly be the start date of the Tour de France in 2016
rider_dob_MATCH_merge$Age <- as.numeric(round((today() - dmy(rider_dob_MATCH_merge$dob))/365, 2))

# Order by 2016 finishing position
rider_dob_MATCH_merge <- rider_dob_MATCH_merge[order(rider_dob_MATCH_merge$X2016_FP), ]
# Create a subset of the top 20 finishing riders
top_20_age <- rider_dob_MATCH_merge[1:20, ]
# View(rider_dob_MATCH_merge)
ggplot(top_20_age, aes(x = Age, y = X2016_FP, colour = Team)) + geom_point() + geom_text(aes(label=Rider),hjust=0, vjust=0)


######################################################################
# Recommencing work on Mon 20 Nov 17
# FUZZY NAME MATCHING FUNCTIONS
# I'm getting back into the above Age analysis with the use of fuzzy
# name matching functions I've developed over the last six weeks.

# Read the original results analysis and database Age tables back into R
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
rider_table_MATCH <- read.csv("rider_table_MATCH.csv", header = TRUE)
View(results_df)
View(rider_table_MATCH)

# Load my tdfAuto package
require(tdfAuto)

# Run two list fuzzy name matching function
pos_match <- levelTwoListMatch(results_df$Rider, rider_table_MATCH$rider_name)

# Combinge search list and input list of riders into a dataframe for comparison
name_check <- data.frame(as.factor(results_df$Rider), as.factor(rider_table_MATCH$rider_name[pos_match]))
colnames(name_check) <- c("Search_Riders", "Input_Riders")
View(name_check)
# The above matching appears to have been executed correctly, with accurate matches
# against each name and an 'NA' entry where no match is found.

# Identify the missing riders (no match found)
name_check[is.na(name_check$Input_Riders) , "Search_Riders"]

# Put the search list and the missing rider list into alphabetical order for a manual check
alpha_db <- rider_table_MATCH %>% arrange(rider_name) %>% select(rider_name)
write.csv(alpha_db, "alpha_db.csv", row.names = FALSE)
alpha_fuzzymatch <- name_check %>% filter(is.na(Input_Riders)) %>% arrange(Search_Riders) %>% select(Search_Riders)
write.csv(alpha_fuzzymatch, "alpha_fuzzymatch.csv", row.names = FALSE)
View(alpha_db)
View(alpha_fuzzymatch)
# A manual check of the two lists confirms there are no riders that should have been matched.
# It would appear the missing riders were not extracted from the database.
# Time to go and refine the database matching and extraction approach.

######################################################################
# Improving Database Fuzzy Name Matching

# Load required packages
require(RMySQL)

# Read the original results analysis and database Age tables back into R
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
rider_table_MATCH <- read.csv("rider_table_MATCH.csv", header = TRUE)

# Put the search list and the missing rider list into alphabetical order for a manual check
alpha_db <- rider_table_MATCH %>% arrange(rider_name) %>% select(rider_name)
alpha_fuzzymatch <- read.csv("alpha_fuzzymatch.csv", header = TRUE)

#          ### WHERE filter based on missing riders ###
# Build a WHERE criteria list of all the riders missing from my original
# MySQL Boolean matching
WHERE_rider <- paste0("MATCH(rider_name) AGAINST('+", gsub(" ", "+", alpha_fuzzymatch$Search_Riders[1]), "')")
for (r in 2:length(alpha_fuzzymatch$Search_Riders)){
  WHERE_rider <- paste0(WHERE_rider, " OR MATCH(rider_name) AGAINST('+", gsub(" ", "+", alpha_fuzzymatch$Search_Riders[r]), "' IN BOOLEAN MODE)")
}
WHERE_rider

# Paste together the criteria for the MySQL query
criteria <- paste0("SELECT * FROM rider_list_master WHERE ", WHERE_rider, ";")

# Execute MySQL query against missing rider search criteria
rider_table_MISSING <- dbGetQuery(conn_local, criteria)
View(rider_table_MISSING)
# Write resulting missing rider list table to local working data directory
write.csv(rider_table_MISSING, "rider_table_MISSING.csv", row.names = FALSE)

# The above returns only one rider (Adrien Niyonshuti), which is an incorrect 
# match presumably for Adrien Petit

# Now to check for individual names in database
criteria <- paste0("SELECT * FROM rider_list_master WHERE ", 
                   paste0("MATCH(rider_name) AGAINST('+", gsub(" ", "+", 
                  alpha_fuzzymatch$Search_Riders[10]), "')"), ";")
criteria <- "SELECT * FROM rider_list_master WHERE MATCH(rider_name) AGAINST('+Greg+Henderson');"
# Execute MySQL query against missing rider search criteria
rider_table_INDIVIDUAL <- dbGetQuery(conn_local, criteria)
rider_table_INDIVIDUAL
# So the issue here is I'm getting a return from the database for Greg Henderson.
# It's returning two names: Greg Van Avermaet & Gregory Henderson
# However Gregory Henderson IS NOT in the full rider list search results (rider_table_MATCH)

# Run two list fuzzy name matching function
pos_match <- levelTwoListMatch(results_df$Rider, rider_table_INDIVIDUAL$rider_name)
results_df$Rider[155]
require(stringr)
looking_for_Greg <- rider_table_MATCH %>% filter(str_detect(rider_name, "Gre")) %>% arrange(rider_name) 
looking_for_Greg$rider_name

# So I need to work out why my Boolean matching isn't returning Gregory Henderson.


