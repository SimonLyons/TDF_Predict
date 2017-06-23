
require(lubridate)
require(dplyr)

# Obtain 'duration' values stored in database
master_time <- dbGetQuery(conn_local, "SELECT * FROM master_results_time LIMIT 10 OFFSET 300;")
View(master_time)
dur05 <-  master_time$Duration[5]
dur10 <-  master_time$Duration[10]
dur05 <- "1089s (~18.15 minutes)"
dur10 <- "1148s (~19.13 minutes)"

# Sadly, lubridate doesn't recognise its own formatted variables once stored as class character in the database
# This function therefore returns an 'NA'
convert_time_string <- as.duration(dur10)

# Efforts to convert database duration string to duration
# Use the following regex function to identify whether a character exists in the string and its location 
# even if the returned value is TRUE/FALSE
regexpr("s", dur10)


# Ths IF statement checks for the existence of the seconds 's' and if TRUE, does the conversion to
# lubridate 'seconds'. It can then be manipulated as a time variable.

dur_to_sec <- function(u){
  if(regexpr("s", u)){
    u <- seconds(substr(u, 0, regexpr("s", u)-1))
  }   # end IF statement checking for seconds, 's'
  return(u)
}   # end function 'dur_to_sec'


# Convert variable 'dur05' as well.
if(regexpr("s", dur05)){
  dur05 <- seconds(substr(dur05, 0, regexpr("s", dur05)-1))
}

# Some practice manipulation.

hour_time <- lubridate::as.duration("5 hours")

seconds(convert_time_string)

new_time <- dur10 - dur05
period(new_time)
new_time <- new_time * 200
