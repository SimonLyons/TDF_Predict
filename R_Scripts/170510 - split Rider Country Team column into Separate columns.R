

require(rvest)
require(tidyr)

# Set Cycling News url and read it into R
cn_url <- "http://www.cyclingnews.com/giro-ditalia/stage-4/results/"
cn_html <- read_html(cn_url)

# Extract the first table
cn_table <- cn_html %>%  
  html_nodes(xpath = "//table") %>% 
  html_table(fill = TRUE, trim = TRUE) %>% 
  .[[1]]

# Remove the nasty special character from the 'Result' column.
# Note - readHTMLTable from the XML package doesn't introduce this character
cn_table$Result <- gsub("Â", NA, cn_table$Result)
# View(cn_table)

# The XML version for reading tables. This doesn't include the header information (unfortunately).
# cn_table <- XML::readHTMLTable(XML::htmlParse(cn_html), header = TRUE)[[1]]

# Split the 'Rider Name (Country) Team' column using the 'separate' function from Hadley's 'tidyr' package
cn_fix <- separate(data = cn_table, into = c("Rider", "Remaining"), sep = " \\(", col = "Rider Name (Country) Team", remove = TRUE, extra = "drop")
cn_fix <- separate(data = cn_fix, into = c("Country", "Team"), sep = "\\) ", col = "Remaining", remove = TRUE, extra = "drop")

# Use my 'text_clean' function to remove special characters
cn_fix$Rider <- text_clean(cn_fix$Rider)
cn_fix$Team <- text_clean(cn_fix$Team)


View(cn_fix)



