# Local machine
conn_local <- dbConnect(MySQL(), user='test_DB_manager', 
                        password='db_manager_45',  dbname='ProCycling', host='localhost')

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

# A bit like the brutal close connections above, it closing everything
dbDisconnect(dbListConnections(MySQL())[[1]])

# Close open queries (doesn't close the connection - very useful)
dbClearResult(dbListResults(conn_local)[[1]])




query4 <- dbSendQuery(conn_local, "SHOW tables;")
new_df4 <- dbFetch(query4, n=-1)
View(new_df4)

race_calendar_2011 <- dbSendQuery(conn_local, "SELECT *
                                FROM race_calendar_2011;")
race_calendar_2011 <- dbFetch(race_calendar_2011, n=-1)
View(race_calendar_2011)



race_calendar_2013 <- dbSendQuery(conn_local, "SELECT *
                                FROM race_calendar_2013;")
race_calendar_2013 <- dbFetch(race_calendar_2013, n=100)
View(race_calendar_2013)

k <- 2007

# Delete 'row_names' column from race_calendar tables
for(k in 2007:2017){
  # Run through years 2007 to 2017 deleting the 'row_names' column
  dbSendQuery(conn_local, paste("ALTER TABLE race_calendar_", k, " DROP COLUMN row_names;", sep=""))
  # Close open queries (doesn't close the connection - very useful)
  dbClearResult(dbListResults(conn_local)[[1]])
}





race_calendar_2013$start_date <- dmy(race_calendar_2013$start_date)
race_calendar_2013$end_date <- dmy(race_calendar_2013$end_date)

class(race_calendar_2005$start_date)
lubridate::dmy(race_calendar_Master$start_date)

month(race_calendar_2013$start_date, label = TRUE, abbr = FALSE)


# Delete tables from Procycling database
# I've chosen NOT to use 'IF EXISTS' statement as I want an error generated
# if the table does not exist
delete_tables <- dbSendQuery(conn_local, "DROP TABLE race_calendar_13;")


# Script to combine individual year race_calendar tables
# into a single table 'race_calendar_Master'

race_calendar_Master <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 9 ))
colnames(race_calendar_Master) <- colnames(race_calendar_2010)
for(k in 2005:2017){
  # Run through years 2005 to 2017 pulling out the entire table
  race_calendar_query <- dbSendQuery(conn_local, paste("SELECT *
                                FROM race_calendar_", k, ";", sep = ""))
  race_calendar <- dbFetch(race_calendar_query, n=-1)

    # Close open queries (doesn't close the connection - very useful)
  dbClearResult(dbListResults(conn_local)[[1]])
  
  # Now add extracted race_calendar table for individual year to Master table
  race_calendar_Master <- rbind(race_calendar_Master, race_calendar)
}
View(race_calendar_Master)

class(race_calendar_2014$start_date)
class(race_calendar_2010$start_date)
dmy(race_calendar_2010$start_date)


# Fix dates in individual year race_calendar tables using 'lubridate'

input_year <- 2011
for(input_year in 2011:2011){
  # Read in calendar dataframe from the ProCycling database
  query <- dbSendQuery(conn_local, paste("SELECT * FROM race_calendar_", input_year, ";", sep = ""))
  # Note the 'n=-1' is required to return all rows, otherwise the database only returns a max of 500.
  calendar_CN <- dbFetch(query, n=-1)  
  View(calendar_CN)
  # Convert string dates to proper date format
  calendar_CN$start_date <- dmy(calendar_CN$start_date)
  calendar_CN$end_date <- dmy(calendar_CN$end_date)
  
  # Write 'race_calendar' dataframe back to ProCycling database
  try(dbWriteTable(conn_local,type = 'UTF-8', name = paste("race_calendar_", input_year, sep = ""), calendar_cn, overwrite = TRUE, row.names = FALSE))
  # Close open queries
  # dbClearResult(dbListResults(conn_local)[[1]])
  
}



