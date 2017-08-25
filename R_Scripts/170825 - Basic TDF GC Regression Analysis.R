
# Load required libraries
library(dplyr)
library(caret)
library(lattice)   # Producing scatterplots

##################################
# 1. Set up dataset
##################################

# Pull in results dataset with final GC results from 2011 to 2016
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
# Set working directory for Work Laptop
# setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016.csv", header = TRUE)
View(results_df)

# Create new dataframe for poulating with new datafields for use in regression analysis. 
input_data <- data.frame("Rider" = results_df$Rider, "FP_2016" = results_df$X2016_FP)

# Calculate the
#     sum of finishing positions
#     mean of finishing positions
#     number of tours completed
#     highest finishing position
#     lowest finishing position
GC_Sum <- rowSums(results_df[ ,3:7], na.rm = TRUE)    # Not required. Just for interest.
input_data$GC_Mean <- rowMeans(results_df[ ,3:7], na.rm = TRUE)
input_data$GC_Complete <- rowSums(!is.na(results_df[ ,3:7]))
input_data$best_GC <- apply(results_df[ ,3:7], 1, min, na.rm = TRUE)
input_data$worst_GC <- apply(results_df[ ,3:7], 1, max, na.rm = TRUE)
View(input_data)

# Create clean version of input data with riders removed that have not
# completed a TDF in any of the input years
input_data_non_NaN <- input_data %>% filter(!is.nan(GC_Mean))
View(input_data_non_NaN)

# Write clean dataframe to local .csv file
write.csv(input_data_non_NaN, "input_data_non_NaN.csv", row.names = FALSE)

##################################
# 2. Create training and testing datasets
##################################

set.seed(1977)
my_split <- createDataPartition(input_data_non_NaN$FP_2016, p = 0.7, list = FALSE)

input_data_train <- input_data_non_NaN[my_split, ]
input_data_test <- input_data_non_NaN[-my_split, ]

# Create scatterplot to assist in visually identifying input
# variables with a strong relationship with predicting final GC position.
splom(input_data_train[ , 2:6 ], data = input_data_train)
# This is interesting as it appears to indicate a strong (linear) relationship
# between FP_2016 and GC_Mean, Best_GC & Worst_GC but not number of complete tours.

##################################
# 3. Use training dataset to perform regression analysis
##################################

# Use caret package to create predictive model based on speficied method.
lrm <- train(data = input_data_train, FP_2016 ~ GC_Mean + best_GC, method = "glm")

summary(lrm)
lrm$finalModel

# Use predictive model to calculate a finishing position using the test data
lrm_results <- predict(lrm, input_data_test, type = 'raw')

# Shift the results into the testing dataframe and create a delta column for comparison
input_data_test$lrm_results <- round(lrm_results, 0)
input_data_test$lrm_delta <- abs(input_data_test$lrm_results - input_data_test$FP_2016)
View(input_data_test)

mean(input_data_test$lrm_delta)   # Calculate mean of the delta (how far the prediction was off)

