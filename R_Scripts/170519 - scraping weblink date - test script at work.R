

require(RMySQL)
require(rvest)
require(lubridate)


race_url <- "http://www.cyclingnews.com/giro-ditalia/stages/"

if(RCurl::url.exists(race_url)){
  download.file(race_url, "race_url.xml")
  race_html <- read_html("race_url.xml")}

race_links <- race_html %>% 
  html_nodes(xpath="//a[contains(@href, '/results')]") %>% 
  html_attrs()

# Extract the stage dates. Convert to correct date format using lubridate.
stage_dates <- race_html %>% 
  html_nodes(xpath="//article/header/time") %>% 
  html_text() %>% 
  mdy()   # Cov
