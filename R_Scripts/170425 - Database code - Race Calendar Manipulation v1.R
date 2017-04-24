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

calendar_cn_2005 <- dbSendQuery(conn_local, "SELECT *
                                FROM calendar_cn_2005;")
calendar_cn_2005 <- dbFetch(calendar_cn_2005, n=100)
View(calendar_cn_2005)



race_calendar_2013 <- dbSendQuery(conn_local, "SELECT *
                                FROM race_calendar_2013;")
race_calendar_2013 <- dbFetch(race_calendar_2013, n=100)
View(race_calendar_2013)


date23 <- race_calendar_2013$start_date[23]
date11 <- race_calendar_2013$start_date[11]
date23-date11
as.Date(date23)
require(lubridate)
(dmy(date23)-dmy(date11))*23
as.Date(dmy(date11))
lubridate::dmy(race_calendar_2013$start_date)

race_calendar_2013$start_date <- dmy(race_calendar_2013$start_date)
race_calendar_2013$end_date <- dmy(race_calendar_2013$end_date)

lubridate::year(race_calendar_2013$start_date)

month(race_calendar_2013$start_date, label = TRUE, abbr = FALSE)


# Delete tables from Procycling database
# I've chosen NOT to use 'IF EXISTS' statement as I want an error generated
# if the table does not exist
delete_tables <- dbSendQuery(conn_local, "DROP TABLE race_calendar_13;")
