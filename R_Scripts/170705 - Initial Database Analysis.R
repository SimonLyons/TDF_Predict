
require(RMySQL)
require(lubridate)
require(dplyr)

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
                        dbname='ProCycling', host='192.168.1.5', port=3306, timeout=3600)

# Show exsiting tables
procycling_tables <- dbGetQuery(conn_local, "SHOW tables;")
View(procycling_tables)

# Retrieve the master Race Weblinks table
master_race_weblinks <- dbGetQuery(conn_local, "SELECT * FROM race_weblinks_master LIMIT 10;")
View(master_race_weblinks)

# Pull the master_results_time table
master_results_time <- dbGetQuery(conn_local, "SELECT * FROM master_results_time;")
nrow(master_results_time)
View(master_results_time)
master_results_time_cdf <- dbGetQuery(conn_local, "SELECT * FROM master_results_time_cdf;")
nrow(master_results_time_cdf)
View(master_results_time_cdf)

# Pull the race_calendar_master table
race_calendar_master <- dbGetQuery(conn_local, "SELECT * FROM race_calendar_master LIMIT 10;")
View(race_calendar_master)

# Pull the race_weblinks_2016 table
new_race_weblinks_master <- dbGetQuery(conn_local, "SELECT * FROM new_race_weblinks_master LIMIT 10;")
View(new_race_weblinks_master)
ncol(new_race_weblinks_master)
colnames(new_race_weblinks_master)


# The following is meant to kill off open processes on the database.
process_list <- dbGetQuery(conn_local, "SHOW PROCESSLIST;")
kill_query <- dbSendQuery(conn_local, "KILL QUERY 4;")


# Create JOIN(ed) table between 'master_race_weblinks' and 'master_results_time' using 'stage_id' as the key
master_join <- dbGetQuery(conn_local, "SELECT t.Pos, t.Rider, t.Country, t.Team, t.Result, t.result_type, t.result_class, t.result_seconds,
                                        t.duration, t.stage_date, w.race_id, t.stage_id
                          FROM master_results_time t JOIN race_weblinks_master w
                          ON t.stage_id = w.stage_id
                          LIMIT 10000;")
View(master_join)
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data")
write.csv(master_join, "master_join.csv", row.names = FALSE)


# Read the stored master_join.csv file
master_join <- read.csv("master_join.csv", header = TRUE)
# It's obvious there are dupicate entries in the master table.
master_join <- unique(master_join)
View(master_join)
glimpse(master_join)
# Convert stage_date (table read from .csv file) to date format
master_join$stage_date <- as_date(master_join$stage_date)

# Practice subsetting using dplyr filter
my_races <- master_join %>% 
  filter(year(stage_date) == '2013' & Country == 'Aus' & grepl("Jack", Rider))
View(my_races)

# Experimenting with fuzzy searches
filter_jack <- master_join %>% 
  filter(agrepl("jack", Rider, max.distance = 0.05))
View(filter_jack)

# BIG AND NASTY
# This is my attempt to join three tables at once, in this case:
# race_calendar_master, master_results_time_cdf, race_calendar_master
#  using 'stage_id' as the key and 'race_id' as the other key
super_join <- dbGetQuery(conn_local, "SELECT t.Pos, t.Rider, t.Country, t.Team, t.Result, 
                                        t.result_type, t.result_class, t.result_seconds,
                                        t.duration, t.stage_date, 
                                        w.race_id, t.stage_id, w.stage, w.date, w.distance,
                                        c.race_details, c.discipline, c.location, c.uci_code, c.start_date, c.end_date
                          FROM new_race_weblinks_master w 
                          LEFT JOIN race_calendar_master c
                          ON w.race_id = c.race_id
                          LEFT JOIN master_results_time t
                          ON t.stage_id = w.stage_id;")
View(super_join)
nrow(super_join)
# Write the results to a local .csv file for future analysis
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/")
write.csv(super_join, "super_join.csv", row.names = FALSE)

super_join <- read.csv("super_join.csv", header = TRUE)

head(super_join)

cf_super <- super_join %>% filter(Rider %in% c("Chris Froome", "Christopher Froome")) %>% 
  filter(race_details == "Tour de France") %>% filter(date > as.Date("2015-01-01") & date < as.Date("2015-12-31"))
View(cf_super)

class(cf_super$date)
super_join$date <- as.Date(super_join$date)



########################################################################
########################################################################

# Subsetting query
# Tour De France, Stage 21 Overall Results for years 2011 through 2016

# date:  year = 2011:2016
# race details:  Tour De France (obviously need to check this)
#                then use it to determine the relevant race_id
# stage_id: This is where we'll isolate to Stage 21 only.

# Need to perform a search of the race_calendar_master_cdf table. Not sure how to perform a fuzzy search in MySQL.

# master_results_time_cdf, race_calendar_master_cdf, race_weblinks_master

# Pull the race_id for Tour de France events between 2011 and 2016
tdf_query <- dbGetQuery(conn_local, "SELECT * 
           FROM race_calendar_master_cdf
           WHERE 
              (YEAR(start_date) >= '2011' AND YEAR(start_date) < '2017') AND
              (race_details = 'tour de france')
                        ;")
View(tdf_query)

# Pull the stage_id for these events from the race_weblinks_master table

# Create the race_weblinks_master table in the database by combining all the individual year race_weblinks tables
delete_tables <- dbSendQuery(conn_local, "DROP TABLE race_weblinks_master;")
my_table <- dbGetQuery(conn_local, "SELECT * FROM race_weblinks_2015 LIMIT 0;")
dbWriteTable(conn_local, type = 'UTF-8', name = "race_weblinks_master", my_table, overwrite = TRUE, row.names = FALSE)

dbSendQuery(conn_local, "INSERT INTO race_weblinks_master
            SELECT * FROM race_weblinks_2005
            UNION
            SELECT * FROM race_weblinks_2006
            UNION
            SELECT * FROM race_weblinks_2007
            UNION
            SELECT * FROM race_weblinks_2008
            UNION
            SELECT * FROM race_weblinks_2009
            UNION
            SELECT * FROM race_weblinks_2010
            UNION
            SELECT * FROM race_weblinks_2011
            UNION
            SELECT * FROM race_weblinks_2012
            UNION
            SELECT * FROM race_weblinks_2013
            UNION
            SELECT * FROM race_weblinks_2014
            UNION
            SELECT * FROM race_weblinks_2015
            UNION
            SELECT * FROM race_weblinks_2016
            UNION
            SELECT * FROM race_weblinks_2017
            ;")

# Need to identify the stage_id for Stage 21 of each Tour De France race.

stage_id_list <- NA   # Create an empty list. It will hold a list of every stage_id associated with each of my race_id's.

# Run a loop, collecting all the stage_id's for each race_id and then taking the last stage_id for each and
# recording it in the existing tdf_query dataframe.
for (r in 1:length(tdf_query$race_id)){
  stage_id_list[r] <- dbGetQuery(conn_local, paste0("SELECT stage_id FROM race_weblinks_master WHERE race_id = '", 
                                                 tdf_query$race_id[r], "';"))
  tdf_query$my_stage_id[r] <- stage_id_list[[r]][[length(stage_id_list[[r]])]]
}
View(tdf_query)

# Build a new dataframe, with the General Classification results for the final stage of the Tour de France
# for each of the years in my query. We'll run a loop through each year, retrieve the final table for each year
# and then row bind them all together for a new, single dataframe with the subsetted data.

final_results_df <- NA
for (y in 1:length(tdf_query$my_stage_id)){
  single_year_df <- dbGetQuery(conn_local, paste0("SELECT * FROM master_results_time WHERE stage_id = '", 
                              tdf_query$my_stage_id[y], "' AND result_class LIKE '%General classification%';"))
  final_results_df <- rbind(final_results_df, single_year_df)
}
final_results_df <- final_results_df %>% filter(!is.na(Rider))   # Get rid of the first row, which is all 'NA'
View(final_results_df)

# Now attempt to do the same with a single query
final_results_df_02 <- dbGetQuery(conn_local, "SELECT *
                             FROM master_results_time
                             WHERE (stage_id = 'race_2011_0741_s22' OR stage_id = 'race_2012_0702_s21' OR 
                              stage_id = 'race_2013_0681_s21' OR stage_id = 'race_2014_0397_s21' OR
                              stage_id = 'race_2015_0240_s21' OR stage_id = 'race_2016_0221_s24')
                              AND result_class LIKE '%General classification%'
                             ;")
View(final_results_df_02)
# 'Pos' column with rider standings needs to be converted to class 'integer'
final_results_df$Pos <- as.integer(final_results_df$Pos)
final_results_df_02$Pos <- as.integer(final_results_df_02$Pos)
final_results_df$stage_date <- as.Date(final_results_df$stage_date)
final_results_df_02$stage_date <- as.Date(final_results_df_02$stage_date)

# Perform a subset to just the podium positions on each dataset for a check (looks good)
podium_spots_01 <- final_results_df %>% 
  filter(Pos < 4)
View(podium_spots_01)

podium_spots_02 <- final_results_df_02 %>% 
  filter(Pos < 4)
View(podium_spots_02)

head(final_results_df)
glimpse(final_results_df)

# Not sure why the 2016 final TDF results have no date, but I'm temporarily insert one:  2016-07-24
final_results_df[final_results_df$stage_id == 'race_2016_0221_s24', 'stage_date'] <- as.Date('2016-07-24')
final_results_df_02[final_results_df_02$stage_id == 'race_2016_0221_s24', 'stage_date'] <- as.Date('2016-07-24')

### Time to build my analysis table ###

# Firstly take the riders and their finishing position in the latest year (2016)
anal_df_2016 <- final_results_df %>%
  filter(year(stage_date) == '2016') %>% 
  select(Rider, Country, Team, "2016_FP" = Pos)

# Next create Rider/Pos tables for each year
anal_df_2015 <- final_results_df %>% filter(year(stage_date) == '2015') %>% select(Rider, "2015_FP" = Pos)
anal_df_2014 <- final_results_df %>% filter(year(stage_date) == '2014') %>% select(Rider, "2014_FP" = Pos)
anal_df_2013 <- final_results_df %>% filter(year(stage_date) == '2013') %>% select(Rider, "2013_FP" = Pos)
anal_df_2012 <- final_results_df %>% filter(year(stage_date) == '2012') %>% select(Rider, "2012_FP" = Pos)
anal_df_2011 <- final_results_df %>% filter(year(stage_date) == '2011') %>% select(Rider, "2011_FP" = Pos)

# Now match up the 2016 riders with their finishing position in previous years
anal_df_2016$`2015_FP` <-  match(anal_df_2016[,1], anal_df_2015[, 1])
anal_df_2016$`2014_FP` <-  match(anal_df_2016[,1], anal_df_2014[, 1])
anal_df_2016$`2013_FP` <-  match(anal_df_2016[,1], anal_df_2013[, 1])
anal_df_2016$`2012_FP` <-  match(anal_df_2016[,1], anal_df_2012[, 1])
anal_df_2016$`2011_FP` <-  match(anal_df_2016[,1], anal_df_2011[, 1])
View(anal_df_2016)

setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
write.csv(anal_df_2016, "anal_df_2016.csv", row.names = FALSE)

##############################################################
# Building second basic dataset, which includes Country and 
# Team details in addition to Rider.

# Firstly take the riders and their finishing position in the latest year (2016)
anal_df_2016 <- final_results_df_02 %>%
  filter(year(stage_date) == '2016') %>% 
  select(Rider, Country, Team, "2016_FP" = Pos)

# Next create Rider/Pos tables for each year
anal_df_2015 <- final_results_df_02 %>% filter(year(stage_date) == '2015') %>% select(Rider, "2015_FP" = Pos)
anal_df_2014 <- final_results_df_02 %>% filter(year(stage_date) == '2014') %>% select(Rider, "2014_FP" = Pos)
anal_df_2013 <- final_results_df_02 %>% filter(year(stage_date) == '2013') %>% select(Rider, "2013_FP" = Pos)
anal_df_2012 <- final_results_df_02 %>% filter(year(stage_date) == '2012') %>% select(Rider, "2012_FP" = Pos)
anal_df_2011 <- final_results_df_02 %>% filter(year(stage_date) == '2011') %>% select(Rider, "2011_FP" = Pos)

# Now match up the 2016 riders with their finishing position in previous years
anal_df_2016$`2015_FP` <-  match(anal_df_2016[,1], anal_df_2015[, 1])
anal_df_2016$`2014_FP` <-  match(anal_df_2016[,1], anal_df_2014[, 1])
anal_df_2016$`2013_FP` <-  match(anal_df_2016[,1], anal_df_2013[, 1])
anal_df_2016$`2012_FP` <-  match(anal_df_2016[,1], anal_df_2012[, 1])
anal_df_2016$`2011_FP` <-  match(anal_df_2016[,1], anal_df_2011[, 1])
View(anal_df_2016)

setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
write.csv(anal_df_2016, "anal_df_2016_C&T.csv", row.names = FALSE)

###############################
###############################
###############################




# CALENDAR TO WEBLINKS JOIN
calendar_weblinks_join <- NA

# Create total JOIN between race_calendar_master and new_race_weblinks_master
calendar_weblinks_join <- dbGetQuery(conn_local, "SELECT w.race_id, w.stage_id, w.stage, w.date, w.distance, 
                                        c.race_details, c.discipline, c.location, c.uci_code, c.start_date, c.end_date
                                     FROM race_calendar_master c
                                     JOIN new_race_weblinks_master w
                                     ON w.race_id = c.race_id;")
View(calendar_weblinks_join)

setwd("/home/a_friend/data_analysis/projects/TDF_Predict/")
write.csv(calendar_weblinks_join, "calendar_weblinks_join.csv", row.names = FALSE)


########################################################################
########################################################################

# THIS SECTION DEALS WITH CORRECTING THE DATE FORMAT

# Retrieve the entire master_results_time table from the database
master_time <- dbGetQuery(conn_local, "SELECT * FROM master_results_time;")

# Correct the date format for stage_date
require(lubridate)
master_time_cdf <- master_time
master_time_cdf$stage_date <- as_date(master_time_cdf$stage_date)
nrow(master_time_cdf)

# Select a subset of the full master_time dataframe
master_time_cdf_reduced <- master_time_cdf[456:3245, ]

# I'm going to create a dummary master_results_time table and attempt to
# convert the stage_date column format to date type. 
dbWriteTable(conn_local, name = "master_results_time_cdf",
             master_time_cdf_reduced, overwrite = FALSE, row.names = FALSE, append = TRUE)

# Now go and convert the format of the stage_date column
dbSendQuery(conn_local, "ALTER TABLE master_results_time_cdf MODIFY stage_date date;")

# Pull the entire new correct date format table from the database (master_results_time_cdf)
master_results_time_retrieve <- dbGetQuery(conn_local, "SELECT * FROM master_results_time_cdf;")
View(master_results_time_retrieve)
class(master_results_time_retrieve$stage_date)  # Confirms stage_date as 'character'

# Retrieve a subset of data from the corrected date format database table using a date filter

# Firstly with a specific date
master_results_time_retrieve <- dbGetQuery(conn_local, "SELECT * 
                          FROM master_results_time_cdf 
                          WHERE stage_date = convert('2009-06-15', date);")

# Next within a range of dates (both appear to work)
master_results_time_retrieve <- dbGetQuery(conn_local, "SELECT * 
                          FROM master_results_time_cdf 
                          WHERE stage_date BETWEEN convert('2009-06-08', date) AND  convert('2009-06-15', date);")
View(master_results_time_retrieve)


# Do the same for the race_calendar_master table. We'll create correct date format (cdf) version as well.
# Firstly pull in the existing race_calendar_master from the database and check it visually
race_calendar_master <- dbGetQuery(conn_local, "SELECT * 
                          FROM race_calendar_master;")
View(race_calendar_master)
# Now write the race_calendar_master data back to the database, but in another table named 'race_calendar_master_cdf'
dbWriteTable(conn_local, name = "race_calendar_master_cdf",
             race_calendar_master, overwrite = FALSE, row.names = FALSE, append = TRUE)
# Now go and convert the format of the start_date and end_date columns
dbSendQuery(conn_local, "ALTER TABLE race_calendar_master_cdf MODIFY start_date date;")
dbSendQuery(conn_local, "ALTER TABLE race_calendar_master_cdf MODIFY end_date date;")


########################################################################
########################################################################

# THIS SECTION DEALS WITH SORTING OUT THE RACE CALENDARS

race_calendar_master <- dbGetQuery(conn_local, "SELECT * 
                          FROM race_calendar_master;")
View(race_calendar_master)
nrow(race_calendar_master)

# Convert 'start_date' to correct date format
race_calendar_master$start_date <- as.Date(race_calendar_master$start_date)
class(race_calendar_master$start_date)

# Create table with number of races for each year from race_calendar_master
race_master_count <- race_calendar_master %>% 
  group_by("Year" = year(start_date)) %>% 
  summarise("No of Races" = n())
View(race_master_count)
race_master_count <- race_master_count[-14, ]

# Create table with number of races for each year from individual year race calendars
# Create blank race_year_count table
race_year_count <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 2))
colnames(race_year_count) <- c("Year_y", "No_of_Races_y")
row_counter <- 1
# Loop through each of the race calendar tables and count the number of rows
for(y in 2005:2017){
  race_calendar_y <- dbGetQuery(conn_local, paste0("SELECT * 
                          FROM race_calendar_", y, ";"))
  race_year_count[row_counter, 1] <- y
  race_year_count[row_counter, 2] <- nrow(race_calendar_y)
  row_counter <- row_counter + 1   # Increase row counter by one
}

# Combine two tables for comparison
master_count <- cbind(race_master_count, race_year_count)
master_count <- master_count %>% mutate("Delta_Count" = `No of Races` - No_of_Races_y)
View(master_count)
# Save to local working directory
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
write.csv(master_count, "master_calendar_count.csv", row.names = FALSE)

########################################################################
########################################################################


# Change the stage_date column to class 'date'
master_join$stage_date <- as.Date(master_join$stage_date)

# Results on a particular date
part_date <- master_join %>% 
  filter(stage_date >= "2010-01-01" & stage_date <= "2012-01-23")
View(part_date)

# It's obvious there are dupicate entries in the master table.
master_join <- unique(master_join)

# Results in a particular year, using lubridate 'year' function.
# This is great as it appears as though lubridate filtering will allow me to 
# perform a large variety of date filtering.
part_year <- master_join %>% 
  filter(year(stage_date) == "2013")
View(part_year)




master_time <- dbGetQuery(conn_local, "SELECT * 
                          FROM master_results_time 
                          WHERE (convert(stage_date,date) BETWEEN convert('2016-06-01', date), AND  convert('2016-12-31', date));")

# This one works! The 'convert' function in MySQL does a good job of converting the text
# date to a correct MySQL date format, so I can then use MySQL functions to filter the date.
date_query_01 <- dbGetQuery(conn_local, "SELECT * 
                          FROM master_results_time 
                          WHERE convert(stage_date,date) = '2017-06-01';")
View(date_query_01)


# Select some data for test conversion of data and insertion back into database
get_headings <- dbGetQuery(conn_local, "SELECT *
                             FROM master_results_time
                           LIMIT 1000
                           OFFSET 20000;")
View(get_headings)

# Create the dummy table
dbSendQuery(conn_local, "CREATE TABLE aa_date_convert (`stage_date` Date);")
# Check to see if table is created
pull_date_convert <- dbGetQuery(conn_local, "SELECT *
           FROM aa_date_convert;")
View(pull_date_convert)

sub_time_f <- dbGetQuery(conn_local, "SELECT stage_date
                       FROM test_test_master_results_time;")
glimpse(sub_time_f)


View(sub_time)
summary(sub_time)
sub_time_d <- as.data.frame(as.list(sub_time))
sub_time_d[,2] <- as_date(sub_time_d[,1])
head(sub_time_d)
glimpse(sub_time_d)
colnames(sub_time_d)[2] <- "date_converted"


sub_time_subset <- sub_time_d %>% 
  filter(date_converted == "2015-03-14")
View(sub_time_subset)
head(sub_time_subset)
glimpse(sub_time_subset)

sub_rider <- dbGetQuery(conn_local, "SELECT Rider
                       FROM test_test_master_results_time;")



LA_select <- dbGetQuery(conn_local, "SELECT *
                        FROM test_test_master_results_time
                        WHERE Rider = 'Lance Armstrong';")
View(LA_select)

PS_select <- dbGetQuery(conn_local, "SELECT *
                        FROM test_test_master_results_time
                        WHERE Rider = 'Peter Sagan';")
View(PS_select)

MC_select <- dbGetQuery(conn_local, "SELECT *
                        FROM test_test_master_results_time
                        WHERE Rider = 'Mark Cavendish';")
View(MC_select)

true_table <- dbGetQuery(conn_local, "SELECT * 
                           FROM master_results_time;")

dbRemoveTable(conn_local, "master_results_points")

nrow(true_table)


require(MASS)
MASS::Boston
Boston$tax
distinct(Boston$tax)
?unique
unique(sub_rider$Rider)

year(sub_time$stage_date[1:10])

james <- c("1", "5", "7", "5", "9", "5", "7", "3", "3", "3", "3")
dplyr::distinct(james)


sub_time$year <- year(sub_time$stage_date)
years_in_results <- unique(sub_time$year)
order(years_in_results)


count_year <- sub_time %>% 
  group_by(year) %>% 
  summarise(n())





nrow(sub_time %>% filter(year == '2013'))


require(lubridate)
filter_time <- year(sub_time$stage_date) %>% 
  distinct()

distinct_(as.list(filter_time[1:100]))
distinct_(filter_time[1:100])
year(sub_time$stage_date[1])

lubridate::year()

View(filter_time)


pro_cycling_table <- dbGetQuery(conn_local, "SHOW TABLES;")

View(master_time)
View(pro_cycling_table)
