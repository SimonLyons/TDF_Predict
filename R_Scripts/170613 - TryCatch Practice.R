

# Set the number of years and the starting year.
a <- 5   # Number of years
y <- 2016   # Starting year

# Set up a dataframe with some test cycling news calendar weblinks
my_url <- as.data.frame(matrix(data = NA, nrow = a, ncol = 2))
colnames(my_url) <- c("Year", "url")
my_url[ , 1] <- y:(y+(a-1))
my_url[ , 2] <- paste0("http://www.cyclingnews.com/road/races/calendar/", my_url[ , 1], "/")

# Set the working directory (for work laptop)
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.1 Sandpit/")

# Prepare a dataframe to collect error messages and locations
# error_counter <- 1
error_list <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 3))
colnames(error_list) <- c("Year", "Error", "Error_Counter")
write.csv(error_list, "error_list.csv", row.names = FALSE)

# Run through a loop downloading the url data and reporting errors and warnings
for(c in 1:nrow(my_url)){

    suppressWarnings(tryCatch({
    download.file(my_url[c, 2], "my_url.xml")
    
  }, warning = function(war) {
    print(paste("MY_WARNING:  ", my_url[c, 1],war))
    print(war)
    error_list <- read.csv(file = "error_list.csv")
    error_counter <- nrow(error_list) + 1
    error_list[error_counter, 1] <- my_url[c, 1]
    error_list[error_counter, 2] <- war
    error_list[error_counter, 3] <- error_counter
    write.csv(error_list, "error_list.csv", row.names = FALSE)
    # print(error_counter)
    # print(error_list)
    return(war)
  # }, error = function(err) {
  #   print(paste("MY_ERROR:  ",err))
  #   error_list <- read.csv(file = "error_list.csv")
  #   error_counter <- nrow(error_list) + 1
  #   error_list[error_counter, 1] <- c
  #   error_list[error_counter, 2] <- err
  #   write.csv(error_list, "error_list.csv", row.names = FALSE)
    # return(err)
  }, finally = {
    
    print(paste0("Through round: ", c, ", for year ", my_url[c, 1]))
  }   # End FINALLY
    
  ))   # End TryCatch
  
}   # End of FOR loop running through my_url list


error_list <- read.csv(file = "error_list.csv")
