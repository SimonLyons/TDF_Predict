
# This is a quick script to combine the individual yearly race_weblinks tables from the database 
# into a single master table. I've just gone and looked at my race weblinks function and it was 
# set up to write to individual year tables. Now that I'm into analysis, I need this data in a single
# master table in order to do efficient queries.

require(RMySQL)
# I got the database password and local connection setup through code
# in my Initial Database Analysis file.

new_race_weblinks_master <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 8))
colnames(new_race_weblinks_master) <- c("Stage", "date", "location", "distance", "stage_url", "stage_id", "race_id", "race_details")

for (y in 2005:2017){
  race_weblinks_y <- dbGetQuery(conn_local, paste0("SELECT * FROM race_weblinks_", y, ";"))
  new_race_weblinks_master <- rbind(new_race_weblinks_master, race_weblinks_y)
}
View(new_race_weblinks_master)

dbWriteTable(conn_local,type = 'UTF-8', name = "new_race_weblinks_master", new_race_weblinks_master,
             overwrite = TRUE, row.names = FALSE)

# Despite pulling this data directly from the database (!!!), when I go to write the combined
# table back to the database it's telling me there's an invalid UTF-8 character string 'Workers'.
# I'll run my text_clean function over the location and race_details columns

new_race_weblinks_master$location <- text_clean(new_race_weblinks_master$location)
new_race_weblinks_master$race_details <- text_clean(new_race_weblinks_master$race_details)
new_race_weblinks_master$Stage <- text_clean(new_race_weblinks_master$Stage)
new_race_weblinks_master$Stage[809] <- "Workersx92 Credit Union Downtown Criterium"

filter_mytable <- new_race_weblinks_master %>% 
  filter(grepl("Super D", Stage))

# This is the cleaning fucntion that apparently sorted out the crappy
for (r in 1:nrow(new_race_weblinks_master)){
  new_race_weblinks_master$Stage[r] <- gsub("[^[:alnum:]///' ]", "", new_race_weblinks_master$Stage[r])
}




new_race_weblinks_master$Stage[8126] <- "Stage 2 Rothrock Coopers Gap"

which(grepl("Skoda Men", new_race_weblinks_master$Stage))

new_race_weblinks_master[809 ,1] <- text_clean(new_race_weblinks_master[809 ,1])
