# This continued work on basic regression analysis extends on my
# earlier script (), this time including data for each rider's
# Country and Team. I additionally will seek to include more
# refined accuracy tests.

# Load required libraries
library(dplyr)
library(caret)
library(lattice)   # Producing scatterplots
library(tidyr)

##################################
# 1. Set up dataset
##################################

# Pull in results dataset with final GC results from 2011 to 2016
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
# Set working directory for Work Laptop
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016_C&T.csv", header = TRUE)
View(results_df)

# Create new dataframe for poulating with new datafields for use in regression analysis. 
input_data <- data.frame("Rider" = results_df$Rider, "Country" = results_df$Country, "Team" = results_df$Team, "FP_2016" = results_df$X2016_FP)

# Calculate the
#     sum of finishing positions
#     mean of finishing positions
#     number of tours completed
#     highest finishing position
#     lowest finishing position
input_data$GC_Mean <- rowMeans(results_df[ ,5:9], na.rm = TRUE)
input_data$GC_Complete <- rowSums(!is.na(results_df[ ,5:9]))
input_data$best_GC <- apply(results_df[ ,5:9], 1, min, na.rm = TRUE)
input_data$worst_GC <- apply(results_df[ ,5:9], 1, max, na.rm = TRUE)

# Create clean version of input data with riders removed that have not
# completed a TDF in any of the input years
input_data_non_NaN <- input_data %>% filter(!is.nan(GC_Mean))
View(input_data_non_NaN)

# Create new variables
#     Mean and highest finishing positions for all riders from each team  
#     Mean and highest finishing positions for all riders from each Country
#     Dummy variables for each country
input_data_non_NaN <- input_data_non_NaN %>% group_by(Team) %>% mutate("Team_Mean" = mean(GC_Mean), "Team_Highest" = min(best_GC))
input_data_non_NaN <- input_data_non_NaN %>% group_by(Country) %>% mutate("Country_Mean" = mean(GC_Mean), "Country_Highest" = min(best_GC))
input_data_non_NaN <- input_data_non_NaN %>% mutate(value = 1, new_col = Country) %>% spread(new_col, value, fill = 0)
View(input_data_non_NaN)

# Write clean dataframe to local .csv file
write.csv(input_data_non_NaN, "input_data_non_NaN.csv", row.names = FALSE)

##################################
# 2. Create training and testing datasets
##################################

setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
input_data_non_NaN <- read.csv("input_data_non_NaN.csv", header = TRUE)

set.seed(1977)
my_split <- createDataPartition(input_data_non_NaN$FP_2016, p = 0.7, list = FALSE)

input_data_train <- input_data_non_NaN[my_split, ]
input_data_test <- input_data_non_NaN[-my_split, ]
View(input_data_train)

# Create scatterplot to assist in visually identifying input
# variables with a strong relationship with predicting final GC position.
splom_data <- input_data_train %>% select(FP_2016, Team_Mean, Team_Highest, Country_Mean, Country_Highest)
View(splom_data[1:10,])
splom(input_data_train[ , 4:8])
splom(splom_data[1:10,])

# This is interesting as it appears to indicate a strong (linear) relationship
# between FP_2016 and GC_Mean, Best_GC & Worst_GC but not number of complete tours.

##################################
# 3. Use training dataset to perform regression analysis
##################################

# Use caret package to create predictive model based on speficied method.
lrm <- train(data = input_data_train, FP_2016 ~ GC_Mean + best_GC + Team_Mean + Country_Mean, method = "lm")
summary(lrm)
lrm$finalModel
lrm$results
