
# Script for checking status of Race Weblink tables in ProCyling Database

# Set working directory to user passwords location
setwd("C:/b_Data_Analysis/Database")
# Read password file
psswd <- read.csv("passwords_db.csv", header = TRUE)
conn_local <- dbConnect(MySQL(), user = as.character(psswd[psswd$type== "Manager", "user"]),
                        password = as.character(psswd[psswd$type == "Manager", "password"]),
                        dbname='ProCycling', host='localhost') 


# Create empty dataframe for storing results of queries
race_weblinks_status <- as.data.frame(matrix(data = NA, 0, ncol = 3 ))
colnames(race_weblinks_status) <- c("Year", "No_Cols", "No_rows")

# Create counter for cycling through rows in the dataframe
counter <- 1

for(s in 2005:2017){
  Race_Weblink_Year <- dbGetQuery(conn_local, paste0("SELECT * FROM race_weblinks_", s, ";"))
  race_weblinks_status[counter, 1] <- s   # First column is the year
  race_weblinks_status[counter, 2] <- ncol(Race_Weblink_Year)   # Second column is the number of columns in the race weblink table
  race_weblinks_status[counter, 3] <- nrow(Race_Weblink_Year)   # Last column has the number of rows, representing the number of races/stages.
  counter <- counter + 1   # Cycle the counter up by one each time through the loop
}

setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files/")
write.csv(race_weblinks_status, file = "race_weblinks_status.csv", row.names = FALSE)

getwd()
