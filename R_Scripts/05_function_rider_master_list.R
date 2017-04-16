# Build a master list of riders
# This function will take all of the annual rider lists
# and combine them, removing duplicates
#
# The tricky bit is likely to be how I define the teams they have belonged
# in each particular year

riderMasterList <- function(start_year, end_year){
    # Set working directory to user passwords location
  setwd("C:/b_Data_Analysis/Database")
  # Read password file
  psswd <- read.csv("passwords_db.csv", header = TRUE)
  # Establish connection to ProCycling database
  conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]) , 
                          password = as.character(psswd[psswd$type == "Manager", "password"]),  
                          dbname='ProCycling', host='localhost')   
  
  table_list <- c()
  e <- 1
  
  # no_years <- length(start_year:end_year)
  master_list <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 6 ))
  
  # Run LOOP through the number of years to fetch from database
  for (y in start_year:end_year){
    query <- dbSendQuery(conn_local, paste("SELECT * FROM riderlist_", y, ";", sep = ""))
    table_list[[e]] <- dbFetch(query)
    dbClearResult(dbListResults(conn_local)[[1]])
    e <- e + 1
  }

  # Combine annual rider list tables into a single rider master list
  master_list <- do.call(rbind, table_list)[ , -1]
  colnames(master_list)[6] <-  "uci_id"
  # Remove duplicate rider entries
  master_list <- master_list[!duplicated(master_list$rider_name), ]
  # Write master_list to database as rider_list_master
  dbWriteTable(conn_local, type = 'UTF-8', name = "rider_list_master", master_list, 
               overwrite = TRUE, row.names=F, nrows = nrow(master_list))

  # Close all database connections
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) 
    dbDisconnect(con)
}   #End FUNCTION riderMasterList