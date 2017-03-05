# Build a master list of riders
# This function will take all of the annual rider lists
# and combine them, removing duplicates
#
# The tricky bit is likely to be how I define the teams they have belonged
# in each particular year

riderMasterList <- function(start_year, end_year){
  
  # Establish connection to ProCycling database
  conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='localhost')
  
  table_list <- c()
  e <- 1
  
  # no_years <- length(start_year:end_year)
  master_list <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 6 ))
  
  # Run LOOP through the number of years
  for (y in start_year:end_year){
    query <- dbSendQuery(conn_local, paste("SELECT * FROM riderlist_", y, ";", sep = ""))
    table_list[[e]] <- dbFetch(query)
    dbClearResult(dbListResults(conn_local)[[1]])
    e <- e + 1
  }

  # Combine annual rider list tables into a single rider master list
  master_list <- do.call(rbind, table_list)[ , -1]
  colnames(master_list)[6] <-  "uci_id"
  # master_list <- within(master_list, rm("team_name"))
  master_list <- master_list[!duplicated(master_list$rider_name), ]
  nrow(master_list)
  head(master_list)
  nrow(master_list[master_list$nationality == "United States", ])
  names(master_list)
  # Write master_list to database as rider_list_master
  dbWriteTable(conn_local, type = 'UTF-8', name = "rider_list_master", master_list, overwrite = TRUE, row.names=F, nrows = nrow(master_list))

    # Close all database connections
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) 
    dbDisconnect(con)
}   #End FUNCTION riderMasterList