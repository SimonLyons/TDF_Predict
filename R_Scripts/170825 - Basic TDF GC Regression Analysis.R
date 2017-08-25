library(dplyr)

# Pull in results dataset with final GC results from 2011 to 2016
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
# Set working directory for Work Laptop
# setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_data/")


results_df <- read.csv("anal_df_2016.csv", header = TRUE)
View(results_df)

# Create new dataframe for poulating with new datafields for use in regression analysis. 

input_data <- data.frame(results_df$Rider)

# Calculate the sum of finishing positions
GC_Sum <- rowSums(results_df[ ,3:7], na.rm = TRUE)

# Calculate the mean of finishing positions
input_data$GC_Mean <- rowMeans(results_df[ ,3:7], na.rm = TRUE)

# Calculate the number of tours completed
input_data$GC_Complete <- rowSums(!is.na(results_df[ ,3:7]))

# Calcuate the highest finishing position
input_data$best_GC <- apply(results_df[ ,3:7], 1, min, na.rm = TRUE)

# Calculate the lowest finishing position
input_data$worst_GC <- apply(results_df[ ,3:7], 1, max, na.rm = TRUE)

View(input_data)

# Create clean version of input data with riders removed that have not
# completed a TDF in any of the input years
input_data_non_NaN <- input_data %>% filter(!is.nan(GC_Mean))
View(input_data_non_NaN)

# Write clean dataframe to local .csv file
write.csv(input_data_non_NaN, "input_data_non_NaN.csv", row.names = FALSE)
