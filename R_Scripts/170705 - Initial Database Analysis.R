
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
master_race_weblinks <- dbGetQuery(conn_local, "SELECT * FROM race_weblinks_master;")
View(master_race_weblinks)

# Pull the master_results_time table
master_results_time <- dbGetQuery(conn_local, "SELECT * FROM master_results_time;")
View(master_results_time)

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


########################################################################
########################################################################

# THIS SECTION DEALS WITH CORRECTING THE DATE FORMAT

# Retrieve the entire master_results_time table from the database
master_time <- dbGetQuery(conn_local, "SELECT * FROM master_results_time;")

# Corect the date format for stage_date
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

# THis one works! The 'convert' function in MySQL does a good job of converting the text
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
