# This function is primarily used to prepare data for 
# writing to the MySQL database, which apparently doesn't
# accept a wide variety of foreign and special characters.

# The intent is to have a single function for application uniformly 
# across text import functions.

# Use function to replace latin and foreign characters with basic ASCII characters
text_clean <- function(string) {
  # Original 'removeDiscritics' function
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿøü"
    ,"SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyyou"
    , string
  )
  
  # My fixes for the variety of character problems
  string <- as.character(string)
  gsub("[[:punct:]]", "", string)
  gsub("[^[:alnum:]///' ]", "", string)
  gsub(rawToChar(as.raw("0xa0")), "", string)
  gsub("  ", " ", string)
   
}

