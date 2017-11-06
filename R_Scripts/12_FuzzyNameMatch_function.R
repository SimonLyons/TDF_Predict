################################################################################
# FuzzyNameMatch Function
################################################################################
# The FuzzyNameMatch function is designed to offer a more refined and
# optimised approach to matching the (person) names between two string
# vectors of names (although I'm sure it could be used on other charactr lists).

# The function uses a Levenshtein distance calculation (specifically the
# LevenshteinSim function from the RecordLinkage package) to find the best
# match. 
# The FuzzyNameMatch goes a step further by taking into account the nature of
# people's names. Its primary weapon in this search is comparing all of the words
# in the each name against all of the words in each name from the list, and
# identifying the best match from each name. This is a step up from just a 
# Levenstein distance calculation on the whole name against a whole name.
# We also need to take into account the order of the words in each name.
# Lastly, where the names being compared have a matching number of words, the
# Levenshtein distance calculation is done in order for each word, rather than
# every word iterated against every word.

# The function is a nested combination of three functions. This allows the 
# sapply function to be rolled through the three types of lists:
# 1. For each single word (in a name), comparing it against the list of words in another name;
# 2. Applying the above for each word in the search name; and
# 3. Applying the above for every name in the search list.

# Load required package(s)
require(RecordLinkage)
require(stringi)


# ## ONE ##
# Calculate the Levenshtein Sim value between an input word and each word in a name.
# This 'levenNameList' function is nested into the 'check_name_length_match' function below.
levenNameList <- function(input_word, input_name){
  return(max(sapply(input_name, levenshteinSim, input_word)))
}


# ## TWO ##
# This 'check_name_length_match' function applies function 'levenNameList' for
# each word in the search name. 
# Where the number of words in the two compared names match, a straight Levenshtein
# distance calculation is performed.
# Where there isn't a match in the number of words, the Levenshtein distance calculation
# is iterated across each word permutation.
# In both cases the mean of each result is returned.
check_name_length_match <- function(input_name, search_name){
  # Clean up input types by removing lists.
  if(class(search_name) == 'list'){
    search_name <- unlist(search_name)
  }
  if(class(input_name) == 'list'){
    input_name <- unlist(input_name)
  }
  # search_name_soundex <-  sapply(strsplit(as.character(search_name), " "), soundex)
  # input_name_list_soundex <- sapply(strsplit(as.character(input_name), " "), soundex)
  # Firstly, check for matching number of words in the pair of rider names
  # If they match, do a straight Levenshtein calculation
  if(length(search_name) == length(input_name)){
    # Here the names each have the same number of words and we will therefore
    # perform a straigh Levenshetin calculation without iterating through each word.
    # Clean each name and covert to lowercase
    search_name <- text_clean(search_name)
    search_name <- tolower(search_name)
    input_name <- text_clean(input_name)
    input_name <- tolower(input_name)
    # Calculate the straight Levenshtein value for the entire names
    mean(levenshteinSim(as.character(search_name), as.character(input_name)))
  } else {
    # If the pair of names do not match in length, calculate the Levenshtein Sim
    # value for each word in the first name against each word in the second name.
    # We want to have the words in the longer name cycle against the words in the shorter name
    # First determine which name is the shortest and which is the longest
    both_names <- list(search_name, input_name)   # Put both names into a single object
    longer_name <- both_names[[match(max(length(both_names[[1]]), length(both_names[[2]])), lapply(both_names, length) )]]
    shorter_name <- both_names[[match(min(length(both_names[[1]]), length(both_names[[2]])), lapply(both_names, length))]]
    # Remove special characters with the 'text_clean' function.
    longer_name <- unlist(lapply(longer_name, text_clean))
    shorter_name <- unlist(lapply(shorter_name, text_clean))
    # Now calculate the mean of the best Levenshtein matches (using the levenNameList function)
    mean(sapply(shorter_name,  levenNameList, longer_name))
    # return(my_new_list)
  }   # End ELSE statement
}   # End 'check_name_length_match' function

# ## THREE ##
# Wrap the above Levenshtein matching functions into a master function that calculates the 
# Levenstein Sim value between the search name and each name in the list
# 'search_name': The name of the rider for whom we are seeking a match from another list of riders.
# 'input_name_list': The list containing names of riders.
levenNameAgainstNameList <- function(search_name, input_name_list){
  # convert both the search name and input_name_list:
  #    remove spaces at the start and end of each name
  #    split the names into separate word strings
  #    convert to lowercase
  search_name_split <-  lapply(strsplit(stri_trim_both(search_name), " "), tolower)   # Split the search name and convert to lowercase
  input_name_list_split <- lapply(strsplit(stri_trim_both(input_name_list), " "), tolower)   # Split list of names and convert to lowercase
  # Calculate the Levenshtein Sim value between the search name and each of the names in the list
  levSim_each_name <- sapply(input_name_list_split, check_name_length_match, search_name_split)
  # Find and return the Levenshtein calculated value against each name in the input_list
  levSim_each_name
}

# ## FOUR ##   It's starting to look like I won't use this function (7 Nov 17)
# Takes the list of Levenshtein values calculated for the entire list and then
# determines which has the highest value (closest match) and
# then returns the position of the closest match.
levenBestMatch <- function(search_name, input_name_list){
  levSim_each_name <- levenNameAgainstNameList(search_name, input_name_list)
  max_pos <- match(max(levSim_each_name), levSim_each_name)
  max_pos
}


# ## FIVE ##
# Take two lists of rider names and determine the positions of the best match.
# Returns no match ('NA') where there is no good match or there are duplicates.
levelTwoListMatch <- function(search_name_list, input_name_list){
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
  
}   # End 'levelTwoListMatch' function
