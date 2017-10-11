
########################################
###### FUZZY STRING MATCHING PRACTICE
########################################

# This script has been created to explore improved methods for fuzzy string matching. Specifically, I'm
# looking for methods to match up the variations in rider names between datasets. Presently I'm looking 
# to overcome the differences in rider names between TdF (2016) results and in my database master rider
# list.

# For the purpose of this exercise I'm going to pull down some rider data from the 2017 TdF. It's almost
# amazing to see there is such variation on the Cycling News website between their official rider start
# list and the rider names listed in the first stage results!

# Load required packages
library(rvest)
require(tidyr)
require(RecordLinkage)

list_cn_url <- "http://www.cyclingnews.com/tour-de-france/start-list/"
cn_stage_1_results_url <- "http://www.cyclingnews.com/tour-de-france/stage-1/results/"


# Set the working directory
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_files/")

########################################
###### Download and extract rider list from CN Stage 1 results
cn_stage_1_results_html <- download.file(cn_stage_1_results_url, "cn_stage_1_results_url.xml")
cn_stage_1_results_html <- read_html("cn_stage_1_results_url.xml")

# Extract the tables from the results html data
cn_stage_1_results_table <- cn_stage_1_results_html %>% 
  html_nodes(xpath = "//table") %>% 
  html_table(fill = TRUE, trim = TRUE)

# Take just the first table (with the stage results)
cn_stage_1_results_table <- as.data.frame(cn_stage_1_results_table[[1]])
head(cn_stage_1_results_table)
# Split Rider (Country) Team column into multiple columns
cn_stage_1_results_table_split <- separate(data = cn_stage_1_results_table, into = c("Rider_2", "Remaining"), 
                          sep = " \\(", col = "Rider Name (Country) Team", remove = TRUE, extra = "drop")
cn_stage_1_results_table_split <- separate(data = cn_stage_1_results_table_split, into = c("Country", "Team"), sep = "\\) ", col = "Remaining", remove = TRUE, extra = "drop")


########################################
###### Download and extract rider list from CN Start List
list_cn_html <- download.file(list_cn_url, "list_cn_url.xml")
list_cn_html <- read_html("list_cn_url.xml")

# Extract all of the rider list data. It's contained in two sets of nodes - odd and even!
cn_start_list <- list_cn_html %>% 
  html_nodes(xpath = "//li[contains(@class, 'team-members')]") %>% 
  html_text() %>% 
  as.data.frame()

# Name the column to assist with the 'separate' action below
colnames(cn_start_list) <- "Results"

# First separate the first position data from the rest
cn_start_list_split <- separate(data = cn_start_list, into = c("Position", "Remaining"), sep = " [:space:]", col = "Results", remove = TRUE, extra = "drop")
# Second separate the Rider name from the Country data
cn_start_list_split <- separate(data = cn_start_list_split, into = c("Rider_1", "Country"), sep = "\\(", col = "Remaining", remove = TRUE, extra = "drop")
head(cn_start_list_split)
# Remove
cn_start_list_split$Country <- gsub("\\)", "", cn_start_list_split$Country)
cn_start_list_rider_list <- cn_start_list_split$Rider_1

########################################
###### Merge Start List dataframe with Stage 1 Results dataframe

head(cn_start_list_split)
head(cn_stage_1_results_table_split)

# So the big challenge is differences between the rider names in the two datasets

# I'm going to use a matching function to insert a column in the start list dataframe
# with the matching names of riders from the stage results dataframe.

# Closest match function - designed to use Levenshtein Distances to match strings.
# https://stackoverflow.com/questions/5721883/agrep-only-return-best-matches
require(RecordLinkage)
ClosestMatch2 = function(string, stringVector){
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
}

# The following lapply() action applies the above function. 
# I'm using the Rider_1 list of riders (from the start list) to match aginast the
# Rider_2 list of riders from the stage results dataframe.
cn_start_list_split$Rider_3 <- as.character(lapply(cn_start_list_split$Rider_1, ClosestMatch2,cn_stage_1_results_table_split$Rider_2))


riders_combined <- merge(x = cn_start_list_split, y = cn_stage_1_results_table_split, by.x = 'Rider_3', by.y = 'Rider_2', all = TRUE)
View(riders_combined)
View(cn_start_list_split)
View(cn_stage_1_results_table_split)
class(cn_start_list_split$Rider)
class(cn_stage_1_results_table_split$Rider)

my_new_list <- ClosestMatch3(cn_start_list_split$Rider_1, cn_stage_1_results_table_split$Rider_2)





########################################
###### @work script to answer forum question
###### Includes work on combining Levenshtein and Soundex functions.

# Closest match function - designed to use Levenshtein Distances to match strings.
# https://stackoverflow.com/questions/5721883/agrep-only-return-best-matches

require(RecordLinkage)
ClosestMatch2 = function(string, stringVector){
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
}

# This is my addition to simply the application of the above string matching
# function to matching a first string (vector) against a second string (vector).

# The function returns a matching character vector of matching names from the
# second character vector list.
# This matching character vector can be inserted into the dataframe from which
# the first list originated and can be used to merge or match data between the
# dataframes.
ClosestMatch3 <- function(stringVector1, stringVector2){
  as.character(lapply(stringVector1, ClosestMatch2,stringVector2))
}


# Multiple Fuzzy String Matching   
# I'm experimenting with better solutions for the question posted on the forum:
# https://insite.web.boeing.com/culture/viewQuestion.do?questionId=291911#answerthis

myText <- "Misspelled sentence: evrythng in trvl doumnt mispeled & trvlrs leav mispelled coments"

myText1<-unlist(strsplit(myText," ")) ## agrep requires this form

search_string <- c("travel", "mispelled")



# The following is the shortform answer I posted on the Boeing R Users forum.
require(RecordLinkage)
rep(myText1, length(search_string))[sapply(search_string, levenshteinSim, myText1) > 0.3]

# The above, with conversion to lowercase included
rep(myText1, length(search_string))[sapply(tolower(search_string), levenshteinSim, tolower(myText1)) > 0.3]

# Using a combination of Soundex and Levenshtein functions (on first testing) seems to produce the best result.
rep(myText1, length(search_string))[sapply(soundex(search_string), levenshteinSim, soundex(myText1)) > 0.9]
soundex(search_string)


cycling_search_term <- "Moreno"
cycling_search_string <- c("Daniel Moreno Fernández", "Daniel Martin", "Daniel Monty", "Daniel Onerom")


soundex(cycling_search_term)
cycling_search_string[levenshteinSim(cycling_search_term, cycling_search_string) == max(levenshteinSim(cycling_search_term, cycling_search_string))]

sapply(soundex(cycling_search_term), levenshteinSim, soundex(cycling_search_string))

soundex("Daniel")
soundex("Moreno")
soundex("Fernández")



########################################
###### Fuzzy string matching with strings split into words

# Split the search term into words
cycling_search_term <- "Daniel M0reno Fernández"
cycling_search_term_split <- strsplit(cycling_search_term, " ")
cycling_search_term_split_soundex <- lapply(cycling_search_term_split, soundex)

# Split the search string into words
cycling_search_string <- c("Daniel Moreno Fernández", "Daniel Martin", "Daniel Monty", "Daniel Onerom")
cycling_search_string_split <- strsplit(cycling_search_string, " ")
cycling_search_string_split <- lapply(cycling_search_string, strsplit, " ")


# Looking at methods for assessing single words in rider names.
length(cycling_search_string_split[])
length(cycling_search_string_split[[1]][[1]])
cycling_search_string_split[[1]][[1]][[2]]

### Find the maximum words for a name from the list of names
# I'd like to find a neater/faster/shorter method for the below. I couldn't
# get lapply() to do the trick for me.
max_len <- 0   # The maximum number of words in a rider's name
name_length <- NULL   # The number of words in each rider's name
for(j in 1:length(cycling_search_string_split)){
  name_length[j] <- length(cycling_search_string_split[[j]][[1]])
  # print(name_length)
  if(name_length[j] > max_len){max_len <- name_length[j]}
}



# Perform soundex matching of soundex search term on split search strings
cycling_search_string_soundex <- NULL
for (k in 1:length(cycling_search_string_split)) {
  cycling_search_string_soundex[[k]] <- lapply(cycling_search_string_split[[k]][[1]], soundex)
}   # End FOR statement
cycling_search_string_soundex[[2]]


########################################
###### I'm up to this bit here. I've got some script below which does the soundex()
# matching for the search name against the first name in the search list.
# I like the method I've employed. If the number of words match, I simply perform
# the Levenshtein Sim calculation on each of the soundex words in order.
# If the number of words don't match, then I throw the whole book of matching, calculating
# the Levenshtein Sim result for each word in the search name against every word in the
# first name of the search list and then picking the top results (reducing it to the length
# of the shortest name)

# Perform soundex() match for search term against first name in cycling search string
cycling_search_term_split_soundex[[1]]  # Search name
first_rider_soundex <- unlist(cycling_search_string_soundex[2])   # First name in list of riders
length_search_name <- length(cycling_search_term_split_soundex[[1]])
length_first_rider_name <- length(first_rider_soundex)
length_shortest_name <- min(length_search_name, length_first_rider_name)
if(length_search_name == length_first_rider_name){
  # Match each of the soundex terms in order using the Levenshtein Sim function
  levenS_match <- NULL
  for (y in 1:length(first_rider_soundex)) {
    levenS_match[y] <- levenshteinSim(cycling_search_term_split_soundex[[1]][y], first_rider_soundex[y])
  }   # End FOR statement
  levenS_mean <- mean(levenS_match)
} else {
  # ELSE
  # Perform a Levenshtein Sim calculation for each/every word
  levenS_match <- NULL
  for(l in 1:length_search_name){
    levenS_match[l] <-max(as.numeric(lapply(first_rider_soundex, levenshteinSim, cycling_search_term_split_soundex[[1]][l])))
  }   # End FOR statement
  # Pick the top number of results, where the number is the shortest name from
  # the two and average the top results.
  levenS_mean <- mean(levenS_match[order(levenS_match, decreasing = TRUE)][1:length_shortest_name])
  
}   # End ELSE statement





levenshteinSim(cycling_search_term_split_soundex[1], )
?append

bool_detect("Daniel", cycling_search_string)

stri_detect(cycling_search_term, coll = cycling_search_string)
fixed(cycling_search_string)
stri_det