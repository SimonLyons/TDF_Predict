
require(rvest)
require(dplyr)

machine_learning_models_url <- "https://topepo.github.io/caret/available-models.html"
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
download.file(machine_learning_models_url, "machine_learning_models_url.xml")

model_table <- "machine_learning_models_url.xml" %>%
  read_html() %>% 
  # html_node(xpath="//div[@class='row']") %>% 
  # html_node(xpath="//div[@id='htmlwidget-4ac25b7d7d3dfc41f8b4']") %>% 
  html_node(xpath="//div[@class='col-sm-12']") %>% 
  html_table()
  
View(model_table)
