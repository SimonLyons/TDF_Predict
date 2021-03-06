% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/12_FuzzyNameMatch_function.R
\name{levenNameAgainstNameList}
\alias{levenNameAgainstNameList}
\title{A Levenshtein Sim Name Against List of Names Function}
\usage{
levenNameAgainstNameList(search_name, input_name_list)
}
\arguments{
\item{string}{input and string list}
}
\value{
numeric output
}
\description{
This 'levenNameAgainstNameList' function wraps the prior Levenshtein matching functions into a master function that calculates the Levenstein Sim value between the search name and each name in the list.
'search_name': The name of the rider for whom we are seeking a match from another list of riders.
'input_name_list': The list containing names of riders.
The Levenshtein result for each name in the list is returned.
}
