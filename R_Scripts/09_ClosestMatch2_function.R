# Closest match function - designed to use Levenshtein Distances to match strings.
# https://stackoverflow.com/questions/5721883/agrep-only-return-best-matches

require(RecordLinkage)
ClosestMatch2 = function(string, stringVector){
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
}