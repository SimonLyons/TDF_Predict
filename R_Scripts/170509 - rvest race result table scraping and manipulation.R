
# 1. Download and extract tables
# 2. Extract Table names
# 3. Split Rider (Country) Team column into multiple columns
# 4. Assign columns for result_classification and result_type ('time' or 'points')
# 5. Modify time-based 'Result' into correct class. lubridate::hms, lubridate::duration
# 6. Fill in empty time values. tidyr::fill
# 7. Add 'Duration' column. dplyr::mutate
# 8. Correct non-finishing entries (e.g. DNF, DNS, DNQ, etc).
# 9. Combine all 'time' tables and 'points' tables into two master tables.



require(rvest)
require(tidyr)
require(lubridate)
require(dplyr)

##################################
# 1. Download and extract tables
##################################

# Set the working directory
setwd("C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files/")

# Set the cycling news race result url and download
my_url <- "http://www.cyclingnews.com/giro-ditalia/stage-2/results/"
download.file(my_url, "my_html.xml")
my_html <- read_html("my_html.xml")

# Read the result tables from the HTML
my_table <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//table") %>% 
  html_table(fill = TRUE, trim = TRUE)

##################################
# 2. Extract Table names
##################################

# Extract the table titles - called 'captions'
my_captions <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//table/caption") %>% 
  html_text()
# my_captions

# Check length of captions against number of tables
# length(my_table)
# length(my_captions)

# First table header - the first 'h4' node
# This is the XML method, which seems pretty quick
# table_header_text <- xpathApply(my_url_parse, "//h4", xmlValue)[1]

first_header <- "my_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//h4") %>% 
  html_text()
first_header <- first_header[[1]]


my_captions <- c(first_header, my_captions)

##################################
# 3. Split Rider (Country) Team column into multiple columns
# 4. Assign columns for result_classification and result_type ('time' or 'points')
##################################

for(t in 1:length(my_table)){
  # Split the 'Rider Name (Country) Team' column using the 'separate' function from Hadley's 'tidyr' package
  my_table[[t]] <- separate(data = my_table[[t]], into = c("Rider", "Remaining"), sep = " \\(", col = "Rider Name (Country) Team", remove = TRUE, extra = "drop")
  my_table[[t]] <- separate(data = my_table[[t]], into = c("Country", "Team"), sep = "\\) ", col = "Remaining", remove = TRUE, extra = "drop")
  
  my_table[[t]][,"result_class"] <- my_captions[t]
  colnames(my_table[[t]])[6] <- "result_type"
  my_table[[t]][ ,6] <- ifelse("pts" %in% my_table[[t]][1 ,4], "pts", "time")
}   # End FOR loop through number of tables, 't'



my_test_table <- my_table[[1]]
# my_test_table <- as.data.frame(lapply(my_test_table, function(y) gsub("Â", "", y)) )
# Kill off the non-breaking spaces in the 'Result' column
my_test_table$Result <- lapply(my_test_table$Result, function(y) gsub("[[:space:]]", NA, y))


##################################
# 5. Modify time-based 'Result' into correct class. lubridate::hms, lubridate::duration
##################################

my_test_table$Result <- as.duration(hms(my_test_table$Result))
View(my_test_table)
# write.csv(my_test_table, file = "my_test_table.csv")


##################################
# 6. Fill in empty time values. tidyr::fill
##################################

my_test_table_new <- my_test_table %>% 
  mutate(Result_new = (cumsum(Result)))
View(my_test_table_new)
?cumsum

A <- c(16, 0,0,0,0,2,0,6,0)
B <- c(0,0,0,0,0,0,0,0,0)

n <- 1
for(n in 1:length(A)){
  B[n] <- sum(A[n],lag(A[n]), na.rm = TRUE)
}

B

c <- apply(cbind(A,lag(A)))



apply(cbind(my_list,lag(my_list)),1,sum,na.rm = TRUE)

apply(cbind(my_test_table$Result, lag(my_test_table$Result)), 1, sum, na.rm = TRUE)

my_list <- c(1:49)
my_list[5:9] <- NA

49 + 0

new_list <- sum(my_list, lag(my_list), na.rm = TRUE)

lapply(my_list, lag())

lag(my_list)

?lag

my_test_table <- my_test_table %>% mutate(Result_2 = (Result + lag(Result)))

View(my_test_table)


my_test_table <- fill(my_test_table, Result, .direction = "down")
                                          
                                          
                                          





colnames(my_test_table)[6] <- "result_type"


crank <- as.data.frame(as.matrix("a" = c(1,2,3), "b" = c(3,2,1)))

iris %>% mutate(simon = Species)


View(my_test_table)





# Now fill in empty time result values

my_test_table <- my_table[[1]]
my_test_table[2:100, "Result"] <- NA

fill(my_test_table, Result, .direction = "down")


my_table[[1]]$Result
?fill
fill(my_table[[1]], Result, .direction = "down")


View(my_table[[1]])

#######################################################################################
# Try rvest on Velogames

velo_url <- "https://www.velogames.com/giro-ditalia/2017/teamroster.php?tid=59092f09eec43613"
download.file(velo_url, "velo_html.xml")

# Read the column names for the table
velo_table_colnames <- "velo_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//thead/th") %>% 
  html_text() %>%
  { gsub("\\r\\n", "", .) }   # There are some messy escape characters that need removal
# The first column name is empty, which we will assign the name 'Rider'
velo_table_colnames[1] <- "Rider"

# Extract the table
velo_table <- "velo_html.xml" %>% 
  read_html() %>% 
  html_nodes(xpath="//table") %>% 
  html_table(fill = TRUE)
velo_table <- velo_table[[2]]

# Assign the correct column names
colnames(velo_table) <- velo_table_colnames

View(velo_table)
