# Download and sort into table data for the variety of packages available to caret

require(rvest)
require(dplyr)
require(tidyr)
library(jsonlite)

# Download HTML data
machine_learning_models_url <- "https://topepo.github.io/caret/available-models.html"

# Set working directory for linux HP laptop
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")

# Set working directory for work laptop
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_files/")

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
my_json_data <- as.data.frame(my_json_data)

# transpose dataframe
my_json_data <- t(my_json_data)
colnames(my_json_data) <- c("Model", "method_Value", "Type", "Libraries", "Tuning Parameters")

View(my_json_data)

# Write .csv file locally
write.csv(my_json_data, "my_json_data.csv", row.names = FALSE)

# Set working directory for linux HP laptop
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
# Read .csv file back into R
my_json_data <- read.csv("my_json_data.csv", header = TRUE)


# I'm attempting to extract the column names from the XML code, but I'm not having much success
model_headings <- "machine_learning_models_url.xml" %>%
  read_html() %>% 
  html_node(xpath="//table[@class='table table-striped table-hover']") %>% 
  html_text()

table_nodes <- "machine_learning_models_url.xml" %>%
  read_html() %>% 
  html_nodes(xpath="//script[@type='application/json']") %>% 
  html_structure()


