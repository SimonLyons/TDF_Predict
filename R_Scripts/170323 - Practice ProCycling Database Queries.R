
# Load libraries
require(RMySQL)

# set working directory
setwd("C:/b_Data_Analysis/Database")

# Read password file
psswd <- read.csv("passwords_db.csv", header = TRUE)
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , password = as.character(psswd[psswd$type == "Manager", "password"]),  dbname='ProCycling', host='localhost')

# Query to show all of the tables in the Pro Cycling database
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , password = as.character(psswd[psswd$type == "Manager", "password"]),  dbname='ProCycling', host='localhost')
query <- dbSendQuery(conn_local, "SHOW tables;")
procyclingdb_tables <- dbFetch(query)
View(procyclingdb_tables)
dbDisconnect(dbListConnections(MySQL())[[1]])

# Query to show all of the columns in the 2016 race calendar
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , password = as.character(psswd[psswd$type == "Manager", "password"]),  dbname='ProCycling', host='localhost')
query <- dbSendQuery(conn_local, "SHOW columns FROM race_calendar_2016;")
race_calendar_columns <- dbFetch(query)
View(race_calendar_columns)
dbDisconnect(dbListConnections(MySQL())[[1]])

# Query to return distinct categories of race discipline in the 2016 race calendar
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , password = as.character(psswd[psswd$type == "Manager", "password"]),  dbname='ProCycling', host='localhost')
query <- dbSendQuery(conn_local, "SELECT DISTINCT discipline
                     FROM race_calendar_2016
                     ORDER BY discipline ASC;")
race_discipline <- dbFetch(query)
View(race_discipline)
dbDisconnect(dbListConnections(MySQL())[[1]])


# Need to convert start_date and end_date to correct format before this will work.
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , password = as.character(psswd[psswd$type == "Manager", "password"]),  dbname='ProCycling', host='localhost')
query <- dbSendQuery(conn_local, "SELECT start_date AS 'Date', race_details AS 'Race Name', location AS 'Location'
                    FROM race_calendar_2016
                    WHERE monthname(start_date) = 'August'  
                    ORDER BY discipline ASC;")
road_august <- dbFetch(query)
View(road_august)
dbDisconnect(dbListConnections(MySQL())[[1]])

# Query to return entire 2016 race calendar
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , password = as.character(psswd[psswd$type == "Manager", "password"]),  dbname='ProCycling', host='localhost')
query <- dbSendQuery(conn_local, "SELECT *
                     FROM race_calendar_2016
                     ORDER BY start_date ASC;")
calendar_2016 <- dbFetch(query)
View(calendar_2016)
dbDisconnect(dbListConnections(MySQL())[[1]])

# Query to return all of the races in August from the 2016 race calendar
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , password = as.character(psswd[psswd$type == "Manager", "password"]),  dbname='ProCycling', host='localhost')
query <- dbSendQuery(conn_local, "SELECT start_date AS 'Date', race_details AS 'Race Name', location AS 'Location'
                    FROM race_calendar_2016
                    WHERE start_date LIKE ('%August%')  
                    ORDER BY discipline ASC;")
road_august <- dbFetch(query)
View(road_august)
dbDisconnect(dbListConnections(MySQL())[[1]])

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)