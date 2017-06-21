
require(lubridate)

# Obtain 'duration' values stored in database
master_time <- dbGetQuery(conn_local, "SELECT * FROM master_results_time LIMIT 10 OFFSET 300;")
View(master_time)
dur05 <-  master_time$Duration[5]
dur10 <-  master_time$Duration[10]
print(dur05)
dur05 <- "1089s (~18.15 minutes)"
print(dur10)
dur10 <- "1148s (~19.13 minutes)"


# Efforts to convert database duration string to duration

gregexpr(pattern = "s", dur10, value = TRUE)
?gregexpr

minutes(as_date("6:03:02"))
as_date()
class(dur10)
lubridate::as_date(dur10, %s %M)
dur10 - dur05
lubridate::as.duration()
?as.Date


substr(dur10, 0, grep(dur10, "s"))




