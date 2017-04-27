##########################################
# Admin code. Connections, queries, etc.
# Local machine connection
conn_local <- dbConnect(MySQL(), user='test_DB_manager', 
                        password='db_manager_45',  dbname='ProCycling', host='localhost')

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

# Close open queries (doesn't close the connection - very useful)
dbClearResult(dbListResults(conn_local)[[1]])


query4 <- dbSendQuery(conn_local, "SHOW tables;")
new_df4 <- dbFetch(query4, n=-1)
View(new_df4)

race_calendar_2011 <- dbSendQuery(conn_local, "SELECT *
                                FROM race_calendar_2017;")
race_calendar_2011 <- dbFetch(race_calendar_2011, n=-1)
View(race_calendar_2011)


race_calendar_2013 <- dbSendQuery(conn_local, "SELECT *
                                FROM race_calendar_2013;")
race_calendar_2013 <- dbFetch(race_calendar_2013, n=100)
View(race_calendar_2013)

rc_master <- dbSendQuery(conn_local, "SELECT * FROM race_weblinks_Master;")
rc_master <- dbFetch(rc_master, n=-1)

View(rc_master)


# Delete tables from Procycling database
# I've chosen NOT to use 'IF EXISTS' statement as I want an error generated
# if the table does not exist
delete_tables <- dbSendQuery(conn_local, "DROP TABLE race_calendar_13;")


k <- 2007
##########################################
# Create Master race_weblinks table and write to database

# Setup empty Master table
race_weblinks_Master <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 5 ))
colnames(race_weblinks_Master) <- colnames(race_weblinks_2007)

# Run loop to extract all of the currently existing race_weblinks
# tables from the database
for(k in 2005:2017){
  # Run through years 2007 to 2017 deleting the 'row_names' column
  race_weblinks_query <- dbSendQuery(conn_local, paste("SELECT * FROM race_weblinks_", k, ";", sep=""))
  race_weblinks <- dbFetch(race_weblinks_query)
  
  # assign(paste("race_weblinks_", k, sep = ""), race_weblinks)
  # View(race_weblinks_Master)
  # Close open queries (doesn't close the connection - very useful)
  dbClearResult(dbListResults(conn_local)[[1]])
  
  # Now add extracted race_weblinks table for individual year to Master table
  race_weblinks_Master <- rbind(race_weblinks_Master, race_weblinks)
}
View(race_weblinks_Master)
# Write 'race_weblinks_Master' dataframe back to ProCycling database
try(dbWriteTable(conn_local,type = 'UTF-8', name = "race_weblinks_Master", race_weblinks_Master, overwrite = TRUE, row.names = FALSE))

# End script to create Master race_weblinks database table
##########################################




##########################################
# Script to combine individual year race_calendar tables
# into a single table 'race_calendar_Master'

# Setup empty Master race_calendar table
race_calendar_Master <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 9 ))
colnames(race_calendar_Master) <- colnames(race_calendar_2011)

# Run loop to extract all of the currently existing race_calendar
# tables from the database
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
# Write 'race_calendar' dataframe back to ProCycling database
try(dbWriteTable(conn_local,type = 'UTF-8', name = "race_calendar_Master", race_calendar_Master, overwrite = TRUE, row.names = FALSE))

# End script to create Master race_calendar database table
##########################################

  
  
  
