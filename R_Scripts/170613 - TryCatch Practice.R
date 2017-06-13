




my_url <- NA
counter <- 1
for(u in 2015:2019){
  my_url[counter] <- paste0("http://www.cyclingnews.com/road/races/calendar/", u, "/")
  counter <- counter + 1
}

setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.1 Sandpit/")


error_counter <- 1
for(c in 1:length(my_url)){
  
  error_list <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 2))
  colnames(error_list) <- c("Year", "Error")

    tryCatch({
    download.file(my_url[c], "my_url.xml")
    
  }, warning = function(war) {
    print(paste("MY_WARNING:  ",war))
    error_list[error_counter, 1] <- c
    error_list[error_counter, 2] <- war
    
  }, error = function(err) {
    print(paste("MY_ERROR:  ",err))
  
  }, finally = {
    
    print(paste0("Through round: ", c))
  }
    
  )   # End TryCatch
  
}   # End of FOR loop running through my_url list
