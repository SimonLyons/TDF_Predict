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
require(dplyr)

list_cn_url <- "http://www.cyclingnews.com/tour-de-france/start-list/"
cn_stage_1_results_url <- "http://www.cyclingnews.com/tour-de-france/stage-1/results/"
cn_stage_11_results_url <- "http://www.cyclingnews.com/tour-de-france/stage-11/results/"

# Set the working directory
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_files/")   # Work laptop
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")   # HP laptop

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
# head(cn_stage_1_results_table)
# Split Rider (Country) Team column into multiple columns
cn_stage_1_results_table_split <- separate(data = cn_stage_1_results_table, into = c("Rider_2", "Remaining"), 
                          sep = " \\(", col = "Rider Name (Country) Team", remove = TRUE, extra = "drop")
cn_stage_1_results_table_split <- separate(data = cn_stage_1_results_table_split, into = c("Country", "Team"), sep = "\\) ", col = "Remaining", remove = TRUE, extra = "drop")


########################################
###### Download and extract rider list from CN Stage 11 results
cn_stage_11_results_html <- download.file(cn_stage_11_results_url, "cn_stage_11_results_url.xml")
cn_stage_11_results_html <- read_html("cn_stage_11_results_url.xml")

# Extract the tables from the results html data
cn_stage_11_results_table <- cn_stage_11_results_html %>% 
  html_nodes(xpath = "//table") %>% 
  html_table(fill = TRUE, trim = TRUE)

# Take just the first table (with the stage results)
cn_stage_11_results_table <- as.data.frame(cn_stage_11_results_table[[1]])
# head(cn_stage_1_results_table)
# Split Rider (Country) Team column into multiple columns
cn_stage_11_results_table_split <- separate(data = cn_stage_11_results_table, into = c("Rider_2", "Remaining"), 
                                           sep = " \\(", col = "Rider Name (Country) Team", remove = TRUE, extra = "drop")
cn_stage_11_results_table_split <- separate(data = cn_stage_11_results_table_split, into = c("Country", "Team"), sep = "\\) ", col = "Remaining", remove = TRUE, extra = "drop")



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
# head(cn_start_list_split)
# Remove
cn_start_list_split$Country <- gsub("\\)", "", cn_start_list_split$Country)
cn_start_list_rider_list <- cn_start_list_split$Rider_1

# Save dataframes/tables locally
write.csv(cn_start_list_split, "cn_start_list_split.csv", row.names = FALSE)
write.csv(cn_stage_1_results_table_split, "cn_stage_1_results_table_split.csv", row.names = FALSE)
write.csv(cn_stage_11_results_table_split, "cn_stage_11_results_table_split.csv", row.names = FALSE)

# Set the working directory
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_files/")   # Work laptop
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")   # HP laptop
# Read in saved versions of the cycling news dataframes
cn_start_list_split <- read.csv("cn_start_list_split.csv", header = TRUE)
cn_stage_1_results_table_split <- read.csv("cn_stage_1_results_table_split.csv", header = TRUE)
cn_stage_11_results_table_split <- read.csv("cn_stage_11_results_table_split.csv", header = TRUE)

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
cn_start_list_split$Rider_3 <- as.character(sapply(cn_start_list_split$Rider_1, ClosestMatch2,cn_stage_1_results_table_split$Rider_2))

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
cycling_search_term <- cn_start_list_split$Rider_1[20]    # "Daniel M0reno Fernández"
cycling_search_term_split <- strsplit(cycling_search_term, " ")
cycling_search_term_split_soundex <- sapply(cycling_search_term_split, soundex)


# Split the search string into words
cycling_search_string <- cn_stage_1_results_table_split$Rider
  # c("Daniel Moreno Fernández", "Daniel Martin", "Daniel Monty", "Daniel Onerom")
# cycling_search_string_split <- strsplit(cycling_search_string, " ")
cycling_search_string_split <- sapply(cycling_search_string, strsplit, " ")

# Looking at methods for assessing single words in rider names.
length(cycling_search_string_split)
length(cycling_search_string_split[[1]])
cycling_search_string_split[[1]][[3]]

### Find the maximum words for a name from the list of names
max(sapply(cycling_search_string_split, length))


########################################
##### Perform soundex matching of soundex search term on split search strings
# Interesing section here. It's where I've learnt to use 'sapply' instead of
# of a FOR loop!!! Big achievement!!
cycling_search_string_soundex <- NULL
for (k in 1:length(cycling_search_string_split)) {
  cycling_search_string_soundex[[k]] <- sapply(cycling_search_string_split[[k]], soundex)
}   # End FOR statement
cycling_search_string_soundex[[3]][[2]]

# Define function to calculate the soundex value for each word in a rider's name
soundexOnName <- function(cycling_search_string_name){
  sapply(cycling_search_string_name, soundex)
}
# Feed a list of names to the soundexOnName function
cycling_search_string_soundex_2 <- sapply(cycling_search_string_split, soundexOnName)

########################################


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


# Rewrite the above using sapply() functions
# first_rider_soundex <- unlist(cycling_search_string_soundex_2[1])
first_rider_soundex <- cycling_search_string_soundex_2[2]
length_search_name <- length(cycling_search_term_split_soundex[[1]])
length_first_rider_name <- length(first_rider_soundex)
length_shortest_name <- min(length_search_name, length_first_rider_name)




# Calcuate the Levenshtein Sim value between an input word and each word in a name
levenNameList <- function(input_word, input_name){
  return(max(sapply(input_name, levenshteinSim, input_word)))
}

# Calculate the mean of the Levenshtein Sim value for each word in an input name 
# against another full name
levenFullNameList <- function(search_name, input_name){
  mean(sapply(input_name ,levenNameList, search_name))
}

# Wrap the above Levenshtein matching functions into a master function that
# calculates the Levenstein Sim value between the search name and each name 
# in the list, determines which has the highest value (closest match) and
# then returns the name with the closest match.
### IT MIGHT BE WORTH RETURNING THE POSITION OF THE BEST MATCH RATHER THAN THE NAME ITSELF ###
# 'search_name': The name of the rider for whom we are seeking a match from another list of riders.
# 'input_name_list': The list containing names of riders.
levenNameAgainstNameList <- function(search_name, input_name_list){
  search_name_soundex <-  sapply(strsplit(search_name, " "), soundex)   # Split the search name and determine soundex value
  input_name_list_soundex <- sapply(strsplit(input_name_list, " "), soundex)   # Split list of names and determine soundex value for each
  # Calculate the Levenshtein Sim value between the search name and each of the names in the list
  levSim_each_name <- sapply(input_name_list_soundex, levenFullNameList, search_name_soundex)
  # Find the position (in the list of names) of the same with the closest match
  max_pos <- match(max(levSim_each_name), levSim_each_name)
  input_name_list[max_pos]   # Return the name with the best match
}

# Test example
levenNameAgainstNameList(cycling_search_term, cycling_search_string[1:4])

cn_start_list_split$Rider_1[5]
cn_stage_1_results_table_split$Rider

# Here I'm testing my new function against the rider list from the 2016 TdF. Results are almost 100%....
levenNameAgainstNameList(cn_start_list_split$Rider_1[73], cn_stage_1_results_table_split$Rider)
# The following riders are throwing up the wrong match:
# "Andriy Grivko": "Andrey Amador Bikkazakova "
# I need to do a full test against the entire list. 

levenFullNameList("Andrey Amador Bikkazakova", "Andrey Amador")
levenFullNameList("Andrey Amador Bikkazakova", "Andriy Grivko")

search_name_AAB <- "Andrey Amador Bikkazakova"
search_name_AAB_soundex <- sapply(strsplit(search_name_AAB, " "), soundex)
search_list <- c("Andriy Grivko", "Andrey Amador")
search_list_soundex <- sapply(strsplit(search_list, " "), soundex)
  
levenFullNameList(search_name_AAB_soundex, search_list_soundex[3:4])



levenNameList("Andrey" , "Andrey Amador")
levenshteinSim("Andrey", "Andrey")

sapply(cn_start_list_split$Rider_1, levenshteinSim, cn_start_list_split$Rider_3)

cn_start_list_split$Rider_4 <- (lapply(cn_start_list_split$Rider_1, levenNameAgainstNameList ,cn_stage_1_results_table_split$Rider_2))

View(cn_start_list_split)
View(cn_stage_1_results_table_split)

View(cn_start_list_split[ ,-3])
# Andrey Amador Bikkazakova, Andrey Amador, Andriy Grivko

########################################

bool_detect("Daniel", cycling_search_string)

stri_detect(cycling_search_term, coll = cycling_search_string)

########################################
# I'm attempting to improve my fuzzy matching function by
# introducing some name length matching for optimisation.
# https://stackoverflow.com/questions/14196696/sapply-with-custom-function-series-of-if-statements

require(RecordLinkage)
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")   # HP laptop
# Read in saved versions of the cycling news dataframes
cn_start_list_split <- read.csv("cn_start_list_split.csv", header = TRUE)
cn_stage_1_results_table_split <- read.csv("cn_stage_1_results_table_split.csv", header = TRUE)
cn_stage_11_results_table_split <- read.csv("cn_stage_11_results_table_split.csv", header = TRUE)

# Split search name/term and convert to Soundex values
cycling_search_term <- cn_start_list_split$Rider_1[7]
cycling_search_term_split <- strsplit(as.character(cn_start_list_split$Rider_1[7]), " ")
cycling_search_term_split_soundex <- sapply(cycling_search_term_split, soundex)
length(cycling_search_term_split[[1]])

# Split the search string into words
cycling_search_string <- cn_stage_1_results_table_split$Rider
cycling_search_string_split <- sapply(as.character(cycling_search_string), strsplit, " ")
# Caculate the number of words (names) in the name of each rider
max(sapply(cycling_search_string_split, length))
length(cycling_search_string_split[[141]])

# The function being fed to an apply() function needs to have the list
# input variable first (even though I though it wouldn't matter).
# input_name:  This variable is going to be fed rider names from the list of riders.
# search_name: This is the rider name against which we're seeking a match.
my_simple_IF_match_function <- function(input_name, search_name){
  input_name <- as.vector(input_name)
  search_name <- as.vector(search_name)
  if(length(search_name[[1]]) == length(input_name)){
    print("Matching Length!")
    print(input_name)
  } else{
    print("No match!")
  }
}

# Test simple function against two rider names
my_simple_IF_match_function(cycling_search_string_split[[141]], cycling_search_term_split)

# Test simple function against list of riders using sapply
View(sapply(cycling_search_string_split, my_simple_IF_match_function, cycling_search_term_split))



######################################################################
# I'm attempting to build the function allowing more refined Levenshtein
# Sim matching by isolating names pairs where the number of words match.
# I think this comparison needs to occur in the (nested) levenFullNameList
# function and below I've currently got it in the equivalent of the final
# levenNameAgainstNameList function.


# ## ONE ##
# Calcuate the Levenshtein Sim value between an input word and each word in a name
# This function has no change from the original above.
levenNameList <- function(input_word, input_name){
  return(max(sapply(input_name, levenshteinSim, input_word)))
}

# ## TWO ##
# This 'check_name_length_match' function will replace the middle 'levenFullNameList'
# function in my list of three nested functions from above
check_name_length_match <- function(search_name, input_name){
  # search_name_soundex <-  sapply(strsplit(as.character(search_name), " "), soundex)
  # input_name_list_soundex <- sapply(strsplit(as.character(input_name), " "), soundex)
  # Firstly, check for matching number of words in the pair of rider names
  # If they match, do a straight Levenshtein calculation
  if(length(search_name) == length(input_name)){
    mean(levenshteinSim(as.character(search_name), as.character(input_name)))
  } else {
    # If the pair of names do not match in length, calculate the Levenshtein Sim
    # value for each word in the first name against each word in the second name.
    mean(sapply(input_name ,levenNameList, search_name))
}   # End ELSE statement
}   # End 'check_name_length_match' function


# ## THREE ##
# The only change is the insertion of the new middle function - 'check_name_length_match'
levenNameAgainstNameList <- function(search_name, input_name_list){
  search_name_soundex <-  sapply(strsplit(as.character(search_name), " "), tolower)   # Split the search name and determine soundex value
  input_name_list_soundex <- sapply(strsplit(as.character(input_name_list), " "), tolower)   # Split list of names and determine soundex value for each
  # Calculate the Levenshtein Sim value between the search name and each of the names in the list
  levSim_each_name <- sapply(input_name_list_soundex, check_name_length_match, search_name_soundex)
  # Find the position (in the list of names) of the same with the closest match
  max_pos <- match(max(levSim_each_name), levSim_each_name)
  input_name_list[max_pos]   # Return the name with the best match
}


cycling_search_string
cycling_search_term <- cn_start_list_split$Rider_1[2]
cycling_search_term
levenNameAgainstNameList(cycling_search_term, cycling_search_string)
cn_start_list_split$Rider_4_pos <- sapply(cn_start_list_split$Rider_1, levenNameAgainstNameList, cycling_search_string)
cn_start_list_split$Rider_5 <- cycling_search_string[cn_start_list_split$Rider_4_pos]
View(cn_start_list_split)
View(cn_start_list_split[ , -3])
# 22 Andrey Amador Bikkazakova Crc Andriy Grivko
cycling_search_string[cn_start_list_split$Rider_4_pos]


cn_stage_1_results_table_split[cn_start_list_split$Rider_4_pos, ]

head(cn_start_list_split)
head(cn_stage_1_results_table_split)

first_merge <- merge(cn_start_list_split, cn_stage_1_results_table_split, by.x = "Rider_1", by.y = "Rider_2", all.x = TRUE, all.y = TRUE)
View(first_merge)
second_merge <- merge(cn_start_list_split, cn_stage_1_results_table_split, by.x = "Rider_5", by.y = "Rider_2", all.x = TRUE, all.y = TRUE)
View(second_merge)


######################################################################
# Testing FuzzyNameMatch function against start list and Stage 11 results
head(cn_stage_11_results_table_split)
cn_start_list_split_clean <- read.csv("cn_start_list_split.csv", header = TRUE)
head(cn_start_list_split_clean)

rider_pos <- sapply(cn_start_list_split_clean$Rider_1, levenNameAgainstNameList, cn_stage_11_results_table_split$Rider_2)
length(rider_pos)
length(cn_start_list_split_clean$Rider_1)   # 198 Riders
length(cn_stage_11_results_table_split$Rider_2)   # 180 Riders

# Insert matched rider list (from Stage 11) into the start list dataframe
cn_start_list_split_clean$Rider_4 <- cn_stage_11_results_table_split$Rider_2[rider_pos]
# View the resulting dataframe with matched riders
View(cn_start_list_split_clean[ , -c(3:4)])

# It's evident there are duplicated riders - as a result of having less riders finish Stage 11 than
# there were riders starting the TdF. Not surprising really. So we have duplicated results because
# my function finds the best match for each of the starting riders. 
# (9) Geraint Thomas, (29) Alejandro Valverde Belmonte, (41) Richie Porte, (67) Manuele Mori, (71) Arnaud Demare, 
# (74) Jacopo Guarnieri, (75) Ignatas Konovalovas, (83) Luke Durbridge, (91) Mark Cavendish, (97) Mark Renshaw,
# (108) Matteo Trentin, (111) Peter Sagan, (115) Rafał Majka, (118) Juraj Sagan, (161) Robert Gesink, (168) Jos van Emden,
# (191) Jon Izagirre Insausti

cn_start_list_split_clean$Rider_4[duplicated(cn_start_list_split_clean$Rider_4)]




levSim_each_name <- levenNameAgainstNameList(cn_start_list_split_clean$Rider_1[23], cn_stage_11_results_table_split$Rider_2)
match(max(levSim_each_name), levSim_each_name)
cn_stage_11_results_table_split$Rider_2[97]

rider_pos <- sapply(cn_start_list_split_clean$Rider_1, levenNameAgainstNameList, cn_stage_11_results_table_split$Rider_2)

# Run the levenBestMatch function which returns the best match for each rider in the list
# from the search list.
levenBestMatch(cn_start_list_split_clean$Rider_1[23], cn_stage_11_results_table_split$Rider_2)
# The above identifies that the best match for every rider in the starting list, independent of
# whether that rider is actually in the Stage 11 results or not.


######################################################################
# Building script to deal with mismatched lengths of rider lists
# 
# At this stage I can think of two methods for identifying (and therefore
# removing) incorrect rider matches:
# 1. Finding duplicates in the returned matching list and only keeping the best match; and
# 2. Only retaining matched names above a specified minimum calculated Levenshtein value.

search_name_list <- (cn_start_list_split$Rider_1)
input_name_list <- cn_stage_11_results_table_split$Rider_2
k <- 26

if(length(search_name_list) == length(input_name_list)){
  # Perhaps insert list cleansing functions as an advanced activity
  # Things like replacing special characters and ensuring/converting
  # lists to correct vector type
  rider_pos <- sapply(input_name_list, levenBestMatch, search_name_list)
  
  } else{
    # Determine which is shorter list
    print("Different Length Lists!")
    
    # Create a dataframe to retain the Levenshtein result and best match name
    name_match_table <- as.data.frame(matrix(data = NA, nrow = length(search_name_list), ncol = 4))
    # class(name_match_table$search_name) <- "factor"
    colnames(name_match_table) <- c("search_name", "input_match", "max_pos", "leven_result")
    search_name_list <- as.character(search_name_list)   # Convert name list to string/character list
    for(k in 1:length(search_name_list)){
      # Calculate the Levenshtein value for the search name against each name in the input_list
      levSim_each_name <- levenNameAgainstNameList(search_name_list[[k]], input_name_list)
      name_match_table$search_name[k] <- search_name_list[k]
      max_pos <- match(max(levSim_each_name), levSim_each_name)
      name_match_table$max_pos[[k]] <- max_pos
      name_match_table$input_match[[k]] <- stri_trim_both(input_name_list[max_pos])
      name_match_table$leven_result[[k]] <- max(levSim_each_name)
    }   # End FOR loop running through search name list
  }   # End ELSE statement for lists that don't match in length

# Right-o.... making progress. I've got a table () with the matching data
# including the max Levenshtein results. From this I can locate the duplicates
# and work out which one is the best match and which one(s) should be removed
# as matches. Below is a non-automated attempt at this.

require(dplyr)
weak_matches <- name_match_table %>% filter(leven_result < 1) %>% arrange(desc(leven_result))
View(weak_matches)

remove_dupes <- name_match_table %>% distinct(input_match, .keep_all = TRUE)
View(remove_dupes)

cn_start_list_split$Rider_1
cn_stage_1_results_table_split$Rider_2
cn_stage_11_results_table_split$Rider_2



# Test levelTwoListMatch name matching function using the TDF 16 rider start list and rider list from Stage 11
t09_test <- levelTwoListMatch(cn_start_list_split$Rider_1, cn_stage_11_results_table_split$Rider_2)
# Combine two rider lists (with new order for Stage 11 riders) to check results
t09_final <- data.frame(as.factor(cn_start_list_split$Rider_1), as.factor(cn_stage_11_results_table_split$Rider_2[t09_test]))
View(t09_final)

# Write results locally for future use
t09_test <- t01_test
write.csv(t01_test, "t01_test.csv", row.names = FALSE)
t01_test <- read.csv("t01_test.csv", header = TRUE)
View(t01_test)

# Combine complete rider data tables, using results order for Stage 11 riders to reorder Stage 11 table
combi_rider_tables <- cbind(cn_start_list_split, cn_stage_11_results_table_split[t09_test, ])
View(combi_rider_tables)


# Testing improved levelTwoListMatch function
search_name_list <- cn_start_list_split$Rider_1
input_name_list <- cn_stage_11_results_table_split$Rider_2

check_name_length_match(c("Rigobert", "Robert"), c("Derek", "Bones"))

                        