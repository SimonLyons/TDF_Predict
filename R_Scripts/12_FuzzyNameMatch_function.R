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
  # search_name_soundex <-  sapply(strsplit(as.character(search_name), " "), soundex)
  # input_name_list_soundex <- sapply(strsplit(as.character(input_name), " "), soundex)
  # Firstly, check for matching number of words in the pair of rider names
  # If they match, do a straight Levenshtein calculation
  if(length(search_name[[1]]) == length(input_name[[1]])){
    # print("Matching Length Names")
    mean(levenshteinSim(as.character(search_name), as.character(input_name[1])))
  } else {
    # If the pair of names do not match in length, calculate the Levenshtein Sim
    # value for each word in the first name against each word in the second name.
    print("Non Matching Length Names")
    # We want to have the words in the longer name cycle against the words in the shorter name
    # First determine which name is the shortest and which is the longest
    both_names <- c(search_name, input_name)   # Put both names into a single object
    longer_name <- both_names[[match(max(length(both_names[[1]]), length(both_names[[2]])), lapply(both_names, length) )]]
    shorter_name <- both_names[[match(min(length(both_names[[1]]), length(both_names[[2]])), lapply(both_names, length))]]
    # No calculate the mean of the best Levenshtein matches (using the levenNameList function)
    mean(sapply(shorter_name,  levenNameList, longer_name))
    # return(my_new_list)
  }   # End ELSE statement
}   # End 'check_name_length_match' function


# ## THREE ##
# Wrap the above Levenshtein matching functions into a master function that
# calculates the Levenstein Sim value between the search name and each name 
# in the list
# 'search_name': The name of the rider for whom we are seeking a match from another list of riders.
# 'input_name_list': The list containing names of riders.
levenNameAgainstNameList <- function(search_name, input_name_list){
  search_name_split <-  lapply(strsplit(stri_trim_both(search_name), " "), tolower)   # Split the search name and convert to lowercase
  input_name_list_split <- lapply(strsplit(stri_trim_both(input_name_list), " "), tolower)   # Split list of names and convert to lowercase

  # Calculate the Levenshtein Sim value between the search name and each of the names in the list
  levSim_each_name <- sapply(input_name_list_split, check_name_length_match, search_name_split)
  # Find and return the position (in the list of names) of the name with the closest match
  # match(max(levSim_each_name), levSim_each_name)   # Return the position of the name with the best match
  levSim_each_name
  # The following is used in leiu of the above if the name of the rider is desired instead
  # of the position (in the vector) of the name of the rider with the best match.
  # max_pos <- match(max(levSim_each_name), levSim_each_name)
  # input_name_list[max_pos]   # Return the name with the best match
}

# Takes the list of Levenshtein values calculated for the entire list and then
# determines which has the highest value (closest match) and
# then returns the position of the closest match.
levenBestMatch <- function(search_name, input_name_list){
  levSim_each_name <- levenNameAgainstNameList(search_name, input_name_list)
  max_pos <- match(max(levSim_each_name), levSim_each_name)
  max_pos
}

