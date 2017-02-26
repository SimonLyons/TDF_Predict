# Initial function for rider information



library(XML)

start_year <- 2000
end_year <- 2000

for (p in start_year:end_year){
  my_url <- paste("http://www.cyclingnews.com/teams/", p, sep = "")
  my_url_parse <- htmlParse(my_url)
  
  team_table_list <- c()   # Create empty team_table list to be populated (in a given year)
  
  # Divide the xml data into team elements, divided by /tr node with 'data-i' inluded
  teams_xml <- xpathApply(my_url_parse, "//div[@class='team_box'][1]/ul/li/h3/a")
  no_teams <- 2  # length(teams_xml)
  
  team_table <- as.data.frame(matrix(data = NA, nrow = no_teams, ncol = 2 ))
  colnames(team_table)[1] <- "team.name"
  colnames(team_table)[2] <- "team.link"
  
  # Use Windows Progress Bar for each year
  total <- 2  # no_teams
  # create progress bar
  pb <- winProgressBar(title = "Team Data Download Progress", label = "0% done",  min = 0,
                       max = total, width = 300)
  
  for (j in 1:no_teams){   # For loop that fills in table with all of the teams
    team_table[j,1] <- xmlValue(teams_xml[[j]])   # Team name
    team_table[j,2] <- paste(xmlAttrs(teams_xml[[j]]), sep = "")   # URL link
  }   # End for loop to run through number of teams
 
  assign(paste("T.", p, sep = ""), team_table)   # Give unique year name to each table of teams
  
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
    colnames(i_team_table)[1] <- "rider.name"
    colnames(i_team_table)[2] <- "rider.link"
    colnames(i_team_table)[3] <-  "team.name"
    colnames(i_team_table)[4] <- "dob"
    colnames(i_team_table)[5] <- "nationality"
    colnames(i_team_table)[6] <-  "uci.ID"
    
    for (k in 1:no_riders){
      i_team_table[k,1] <- xmlValue(riders_xml[[k]])   # Team name
      i_team_table[k,2] <- paste(xmlAttrs(riders_xml[[k]]), sep = "")   # URL link  
      i_team_table[k,3] <-  team_table[e, 1]

      # Extract individual rider data from their url
      rider_url <- paste("http://www.cyclingnews.com", i_team_table[k, 2], sep = "")
      rider_url_parse <- htmlParse(rider_url)
      # i_rider_xml <- xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']")
      
      # i_team_table[k,4] <- xmlValue(i_rider_xml[[1]][[2]][[2]])   # Get rider DOB
      i_team_table[k,4] <- xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']/div[@class='rider-data rider-dob']/span[@itemprop='birthDate']", xmlValue)
      # i_team_table[k,5] <- xmlValue(i_rider_xml[[1]][[3]][[2]])   # Get rider nationality
      i_team_table[k,5] <- xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']/div[@class='rider-data rider-nationality']/span[@itemprop='nationality']", xmlValue)
      # i_team_table[k,6] <- xmlValue(i_rider_xml[[1]][[4]][[2]])   # Get rider UCI ID/number
      if(length(xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']/div[@class='rider-data rider-uci']/span[@itemprop='nationality']", xmlValue))>0){
        i_team_table[k,6] <- xpathApply(rider_url_parse, "//div[@class='rider-info-boxout']/div[@class='rider-data rider-uci']/span[@itemprop='nationality']", xmlValue)
        } else {
          i_team_table[k,6] <- xmlValue(i_rider_xml[[1]][[4]][[2]])
      }
      
    }   # End LOOP through number of riders in each team
    
    assign(paste("T.", p, ".", e, sep = ""), i_team_table)
    
    # team_table_list <- c(team_table_list, paste("T.", p, ".", e, sep = ""))
    
  }   # End LOOP through the number of teams in the table of teams (in a given year)
  close(pb)   # Windows Progress Bar script 
  
  # rider_list <- rbind(team_table_list)
  
}   # End FOR loop to run through years


