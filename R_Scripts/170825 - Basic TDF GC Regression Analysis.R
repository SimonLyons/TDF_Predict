



# Pull in results dataset with final GC results from 2011 to 2016
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
# Set working directory for Work Laptop
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_data/")


results_df <- read.csv("anal_df_2016.csv", header = TRUE)
View(results_df)

# Calculate the sum of finishing positions
GC_Sum <- rowSums(results_df[ ,2:7], na.rm = TRUE)

# Calculate the mean of finishing positions
GC_Mean <- rowMeans(results_df[ ,2:7], na.rm = TRUE)

# Calculate the number of tours completed
GC_Sum <- rowSums(!is.na(results_df[ ,2:7]))

# Calcuate the highest finishing position
apply(results_df[ ,2:7], 1, min, na.rm = TRUE)

# Calculate the lowest finishing position
apply(results_df[ ,2:7], 1, n, na.rm = TRUE)

