################################################################################
# Text Clean Function
################################################################################
#
#


#' A Text Clean Function
#'
#' Remove latin, foreign and special characters and replace them with standard western characters.
#' The intent is to have a single function for application uniformly across text import functions.
#' This function is primarily used to prepare data for writing to the MySQL database, which apparently doesn't accept a wide variety of foreign and special characters.
#' @param string input
#' @return string output
#' @export
#' @examples
#' text_clean("Yohann Gène")  ->  "Yohann Gene"
#' text_clean("Michał Périchon")   ->   "Michal Perichon"

text_clean <- function(string) {
  # Load required package(s)
  require(magrittr)
  require(stringi)

  # Original 'removeDiscritics' function
  string <- chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿøü"
    ,"SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyyou"
    , string
  )

  string <- string %>%
    stri_trans_general("LATIN-ASCII") %>%
    as.character() %>%
    gsub("[[:punct:]]", " ", .) %>%
    gsub("[^[:alnum:]///' ]", "", .) %>%
    gsub(rawToChar(as.raw("0xa0")), "", .) %>%
    gsub("  ", " ", .)
}

