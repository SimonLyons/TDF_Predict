
require(rvest)
require(dplyr)

machine_learning_models_url <- "https://topepo.github.io/caret/available-models.html"
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
download.file(machine_learning_models_url, "machine_learning_models_url.xml")

model_table <- "machine_learning_models_url.xml" %>%
  read_html() %>% 
  html_node(xpath="//div[@class='row']") %>% 
  html_table()
  
View(model_table)
