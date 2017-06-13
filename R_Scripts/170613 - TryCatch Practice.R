


# Set up a dataframe with some test cycling news calendar weblinks
my_url <- as.data.frame(matrix(data = NA, nrow = 3, ncol = 2))
colnames(my_url) <- c("Year", "url")
my_url[ , 1] <- 2013:2015
my_url[ , 2] <- paste0("http://www.cyclingnews.com/road/races/calendar/", my_url[ , 1], "/")

# Set the working directory
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.1 Sandpit/")


# Prepare a dataframe to collect error messages and locations
error_counter <- 1
error_list <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 2))
colnames(error_list) <- c("Year", "Error")


# Run through a loop downloading the url data and reporting errors and warnings
for(c in 1:length(my_url)){

    suppressWarnings(tryCatch({
    download.file(my_url[c, 2], "my_url.xml")
    
  }, warning = function(war) {
    print(paste("MY_WARNING:  ", my_url[c, 1],war))
    error_list[error_counter, 1] <- c
    error_list[error_counter, 2] <- war
    error_counter <- error_counter + 1
    return(war)
  }, error = function(err) {
    print(paste("MY_ERROR:  ",err))
    error_list[error_counter, 1] <- c
    error_list[error_counter, 2] <- err
    error_counter <- error_counter + 1
    return(err)
  }, finally = {
    
    print(paste0("Through round: ", c))
    
  }   # End FINALLY
    
  ))   # End TryCatch
  
}   # End of FOR loop running through my_url list
