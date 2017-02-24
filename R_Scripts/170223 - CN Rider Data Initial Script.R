# Initial function for rider information



library(XML)

p <- 2000


for (p in start_year:end_year){
  my_url <- paste("http://www.cyclingnews.com/teams/", p, sep = "")
  my_url_parse <- htmlParse(my_url)
  
  # Divide the xml data into team elements, divided by /tr node with 'data-i' inluded
  teams_xml <- xpathApply(my_url_parse, "//div[@class='team_box'][1]/ul/li/h3/a")
  no_teams <- length(teams_xml)
  
  team_table <- as.data.frame(matrix(data = NA, nrow = length(no_teams), ncol = 2 ))
  colnames(team_table)[1] <- "team.name"
  colnames(team_table)[2] <- "team.link"
  
  for (j in 1:no_teams){   # For loop that fills in table with all of the teams
    team_table[j,1] <- xmlValue(teams_xml[[j]])   # Team name
    team_table[j,2] <- paste(xmlAttrs(teams_xml[[j]]), sep = "")   # URL link
  }   # End for loop to run through number of teams
  
  
  
  
}   # End FOR loop to run through years

