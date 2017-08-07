# Download and sort into table data for the variety of packages available to caret

require(rvest)
require(dplyr)
library(jsonlite)

# Download HTML data
machine_learning_models_url <- "https://topepo.github.io/caret/available-models.html"
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
download.file(machine_learning_models_url, "machine_learning_models_url.xml")

# Read in the text containing the json table data
model_table <- "machine_learning_models_url.xml" %>%
  read_html() %>% 
  html_nodes(xpath="//script[@type='application/json']") %>% 
  html_text()

# Convert into more useable object (still don't know what this is)
my_json <- model_table %>% fromJSON(flatten = FALSE)

# Take data only
my_json_data <- my_json$x$data
# Convert matrix into dataframe

# NEXT SECTION THAT REQUIRES WORK




View(model_table)

# Treating data as JSON and using something other than 'rvest' to retrieve data:
library(httr)

library(rtypeform)

res <- GET(machine_learning_models_url)

status_code(res)
headers(res)
stringi::stri_enc_detect(content(res, "raw"))
str(content(res, 'text', encoding = "ISO-8859-1")) %>% fromJSON(flatten = FALSE)


out <- content(res, as = "text") %>% fromJSON(flatten = FALSE)

out <- res %>% get_results("4ac25b7d7d3dfc41f8b4")

out <- model_table %>% fromJSON(flatten = FALSE)

content(model_table, as = 'text')


yelp <- flatten(model_table)
