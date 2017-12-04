



# Load required packages
require(dplyr)
require(ggplot2)

# Set working directory to location on work laptop
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_data/")
# Read prior file with details of riders finishing the 2016 TdF
rider_duration <- read.csv("rider_duration.csv", header = TRUE)
rider_duration_spread <- read.csv("rider_duration_spread.csv", header = TRUE)
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
date_table <- read.csv("date_table.csv", header = TRUE)
combined_rider_data <- merge(results_df, rider_duration_spread, by.x = "Rider", by.y = "Rider", all = TRUE)
View(combined_rider_data)

# Order combined_rider_data by 2016 finishing position
top_20 <- combined_rider_data %>% arrange(X2016_FP) %>% filter(row_number() %in% 1:20)
View(top_20)

# Create variables for insertion into plot function based on selected year
year_select <- 2014
var_FP <- paste0("X", year_select, "_FP")   # The finishing position column for the selected year
var_HR <- paste0("X", year_select)   # The number of hours ridden for the selected year

# Create the plot of finishing position versus the hours ridden (for selected year)
ggplot(top_20, aes_string(var_FP, var_HR, colour = "Team")) + 
  geom_point() + theme(legend.position="bottom") + geom_text(aes(label=Rider),hjust=0, vjust=0)

