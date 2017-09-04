
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
setwd("C:/aa Simon Lyons/2.0 Work/4.0 Data Analysis/4.6 Projects/TDF_Predict/working_data/")
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

setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
input_data_non_NaN <- read.csv("input_data_non_NaN.csv", header = TRUE)

set.seed(1977)
my_split <- createDataPartition(input_data_non_NaN$FP_2016, p = 0.7, list = FALSE)

input_data_train <- input_data_non_NaN[my_split, ]
input_data_test <- input_data_non_NaN[-my_split, ]
View(input_data_train)

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
lrm$residuals

# Here's a bunch of other models using alternative regression analysis packages
ANFIS_lrm <- train(data = input_data_train, FP_2016 ~ GC_Mean + best_GC, method = "ANFIS")

BstLm_lrm <- train(data = input_data_train, FP_2016 ~ GC_Mean + best_GC, method = "BstLm")

GFS.THRIFT_lrm <- train(data = input_data_train, FP_2016 ~ GC_Mean + best_GC, method = "GFS.THRIFT")

rf_model <- train(data = input_data_train, FP_2016 ~ ., method = "rf")
rf_model$results


basic_lm <- train(data = input_data, FP_2016 ~ GC_Mean + best_GC, method = "lm")



# Use predictive model to calculate a finishing position using the test data
lrm_results <- predict(lrm, input_data_test, type = 'raw')
ANFIS_results <- predict(ANFIS_lrm, input_data_test, type = 'raw')
BstLm_results <- predict(BstLm_lrm, input_data_test, type = 'raw')
GFS.THRIFT_results <- predict(GFS.THRIFT_lrm, input_data_test, type = 'raw')
rf_results <- predict(rf_model, input_data_test, type = 'raw')

# Shift the results into the testing dataframe and create a delta column for comparison
input_data_test$lrm_results <- round(lrm_results, 0)
input_data_test$lrm_delta <- abs(input_data_test$lrm_results - input_data_test$FP_2016)

input_data_test$ANFIS_results <- round(ANFIS_results, 0)
input_data_test$ANFIS_delta <- abs(input_data_test$ANFIS_results - input_data_test$FP_2016)

input_data_test$BstLm_results <- round(BstLm_results, 0)
input_data_test$BstLm_delta <- abs(input_data_test$BstLm_results - input_data_test$FP_2016)

input_data_test$GFS.THRIFT_results <- round(GFS.THRIFT_results, 0)
input_data_test$GFS.THRIFT_delta <- abs(input_data_test$GFS.THRIFT_results - input_data_test$FP_2016)

input_data_test$rf_results <- round(rf_results, 0)
input_data_test$rf_delta <- abs(input_data_test$rf_results - input_data_test$FP_2016)


View(input_data_test)


##################################
# 4. Determine accuracy of results
##################################

mean(input_data_test$lrm_delta)   # Calculate mean of the delta (how far the prediction was off)
mean(input_data_test$ANFIS_delta)
mean(input_data_test$BstLm_delta)
mean(input_data_test$GFS.THRIFT_delta)
mean(input_data_test$rf_delta)

input_data_train_diff <- input_data_train %>% 
  mutate(mean(diff(as.integer([1 ,5:7]))))


mean(diff(as.integer(results_df[2,3:7])), na.rm = TRUE)

class(results_df$X2015_FP)

diff(as.matrix(results_df[5,3:7]))
diff(c(45, 23, 19, 34, 72))
