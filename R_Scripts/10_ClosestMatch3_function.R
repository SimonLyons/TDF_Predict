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