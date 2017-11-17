################################################################################
# tdfAuto Package Update Function
################################################################################
# The tdfAuto_update function is designed to reduce the effort required
# to update my 'tdfAuto' package with my latest changes.
# Using the 'devtools' package this function regenerates the documentation
# and installs all the latest script changes to the functions in my 'tdfAuto'
# package.

# Script to update my 'tdfAuto' package
#'
#' This function will update the documentation and install (locally) the latest
#' local changes to the tdfAuto package, using devtools.
#' @param
#' @return Nothing to return
#' @export
#' @examples


tdfAuto_update <- function(){
  require(devtools)
  setwd("/home/a_friend/data_analysis/projects/TDF_Predict/tdfAuto/")
  document()
  setwd("..")
  install("tdfAuto")
}
?levenBestMatch
