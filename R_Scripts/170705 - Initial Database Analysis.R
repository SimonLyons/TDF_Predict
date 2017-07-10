
require(RMySQL)
require(lubridate)
require(dplyr)

# Setup database connection
dbpsswddir <- set_db_password()
setwd(dbpsswddir)
psswd <- read.csv("passwords_db.csv")
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='192.168.1.5', port=3306, timeout=3600) 

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]), 
                        password = as.character(psswd[psswd$type == "Manager", "password"]),  
                        dbname='ProCycling', host='192.168.1.5', port=3306, timeout=3600)



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
