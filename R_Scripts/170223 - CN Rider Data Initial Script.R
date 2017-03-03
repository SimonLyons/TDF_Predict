# Initial function for rider information
# The function builds a dataframe of all the riders in each year along with
# a range of related information: team, DOB, nationality, etc

# Specify years for extraction of rider data
# ONLY NEEDED FOR TESTING
# start_year <- 2010
# end_year <- 2010

getRiderList <- function(start_year, end_year){

# Require the XML and data.table packages
library(XML)
require(data.table)   # Allows me to use the rbindlist function towards the end
require(RMySQL)   # Package required for writing tables to database

# Create connection to Procycling database
conn_local <- dbConnect(MySQL(), user='test_DB_manager', password='db_manager_45',  dbname='ProCycling', host='localhost')

# Run a FOR loop through all of the specified years. 
for (p in start_year:end_year){
  my_url <- paste("http://www.cyclingnews.com/teams/", p, sep = "")
  my_url_parse <- htmlParse(my_url)
  
  team_table_list <- c()   # Create empty team_table list to be populated (in a given year)
  
  # Divide the xml data into team elements, divided by /tr node with 'data-i' inluded
  teams_xml <- xpathApply(my_url_parse, "//div[@class='team_box'][1]/ul/li/h3/a")
  no_teams <- length(teams_xml)
  
  team_table <- as.data.frame(matrix(data = NA, nrow = no_teams, ncol = 2 ))
  colnames(team_table)[1] <- "team_name"
  colnames(team_table)[2] <- "team.link"
  
  # Use Windows Progress Bar for each year
  total <- no_teams
  # create progress bar
  pb <- winProgressBar(title = paste(p, " Team Data Download Progress", sep = ""), label = "0% done",  min = 0,
                       max = total, width = 300)
  
  for (j in 1:no_teams){   # For loop that fills in table with all of the teams
    team_table[j,1] <- xmlValue(teams_xml[[j]])   # Team name
    team_table[j,2] <- paste(xmlAttrs(teams_xml[[j]]), sep = "")   # Team URL link
  }   # End for loop to run through number of teams
 
  # assign(paste("T.", p, sep = ""), team_table)   # Give unique year name to each table of teams
  
  no_teams <- nrow(team_table)   # re-assign number of teams variable to table length (just to be safe)
  # Insert loop to run through all of the teams in the table of teams
  for (e in 1:no_teams){
    Sys.sleep(0.1)
    setWinProgressBar(pb, e, title = , label = paste( round(e/total*100, 0),
                                                      "% done"))
    team_url <- paste("http://www.cyclingnews.com/", team_table[e, 2], sep = "")
    my_url_parse <- htmlParse(team_url)
    riders_xml <- xpathApply(my_url_parse, "//div[@class='rider']/a")
    no_riders <- length(riders_xml)
    # Create individual team table to insert all rider names and their url link
    i_team_table <- as.data.frame(matrix(data = NA, nrow = no_riders, ncol = 6 ))
    colnames(i_team_table)[1] <- "rider_name"
    colnames(i_team_table)[2] <- "rider_link"
    colnames(i_team_table)[3] <-  "team_name"
    colnames(i_team_table)[4] <- "dob"
    colnames(i_team_table)[5] <- "nationality"
    colnames(i_team_table)[6] <-  "uci_ID"
    
    # sleep
    sleep <- abs(rnorm(1)) + runif(1, 0, .25)
    message("I have done ", e, " of ", no_teams,
            " - gonna sleep ", round(sleep, 2),
            " seconds.")
    Sys.sleep(sleep)
    
    for (k in 1:no_riders){
      i_team_table[k,1] <- xmlValue(riders_xml[[k]])   # Rider name
      i_team_table[k,1] <- removeDiscritics(i_team_table[k,1])
      i_team_table[k,2] <- paste(xmlAttrs(riders_xml[[k]]), sep = "")   # URL link  
      i_team_table[k,3] <-  team_table[e, 1]

      # Extract individual rider data from their url
      rider_url <- paste("http://www.cyclingnews.com", i_team_table[k, 2], sep = "")
      try(rider_url_parse <- htmlParse(rider_url))
      
      # Get rider DOB
      DOB <- xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']/div[@class='rider-data rider-dob']/span[@itemprop='birthDate']", xmlValue)
      if(!is.null(DOB)){i_team_table[k,4] <- DOB} else {i_team_table[k,4] <- NA}
      # Get rider nationality
      nationality <- xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']/div[@class='rider-data rider-nationality']/span[@itemprop='nationality']", xmlValue)
      if(!is.null(nationality)){i_team_table[k,5] <- nationality} else {i_team_table[k,5] <- NA}
      # Get rider UCI ID/number
      uci_pre <- xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']/div[@class='rider-data rider-uci']/span[@class='title']", xmlValue)
      full_uci <- xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']/div[@class='rider-data rider-uci']", xmlValue)
      if(!is.null(uci_pre)){i_team_table[k,6] <- gsub(uci_pre, "", full_uci)} else {i_team_table[k,6] <- NA}
      
    }   # End LOOP through number of riders in each team
    
    # Add latest i_team_table to a list of tables for the year
    team_table_list[[e]] <- assign(paste("T.", p, ".", e, sep = ""), i_team_table)

  }   # End LOOP through the number of teams in the table of teams (in a given year)
  close(pb)   # Windows Progress Bar script 

  # Combine the list of tables for the year into a single list of riders
  # Now inserted into dbWriteTable function below
  # assign(paste("riderlist", p, sep = "_"), do.call(rbind, team_table_list))              # rbindlist(team_table_list))

  # Write the combined list (for each year) to database
  dbWriteTable(conn_local, type = 'UTF-8', name = paste("riderlist", p, sep = "_"), value = assign(paste("riderlist", p, sep = "_"), do.call(rbind, team_table_list)), overwrite = TRUE)
  
}   # End FOR loop to run through years

# Script for closing all active connections to MySQL databases.
all_cons <- dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)

}   # End getRiderList Function