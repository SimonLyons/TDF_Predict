
library(caret)
library(ggplot2)
library(rpart)

# Pull in results dataset with final GC results from 2011 to 2016
setwd("/home/a_friend/data_analysis/projects/TDF_Predict/working_data/")
results_df <- read.csv("anal_df_2016.csv", header = TRUE)

# Set up a training and test set
set.seed(1977)
InTrain <- createDataPartition(y = results_df$X2016_FP, p = 0.7, list = FALSE)   # Create random selection for dividing original data

# Assign 30% of the data to the test set and 70% to the training set
results_test <- results_df[-InTrain, ]
results_train <- results_df[InTrain, ]

mostly_data<-results_df[apply(is.na(results_df),1,sum)<1, ]
InTrain_mostlydata <- createDataPartition(y = mostly_data$X2016_FP, p = 0.7, list = FALSE)
mostlydata_train <- mostly_data[InTrain_mostlydata, ]
mostlydata_test <- mostly_data[-InTrain_mostlydata, ]
length(mostly_data$Rider)

# Create the random forest model based on the training set and using all of the GC results
rf_model <- train(data = mostlydata_train, X2016_FP~., method = 'rf', na.action = na.pass)
train_results <- predict(rf_model, mostlydata_test, type = "raw")
head(train_results)

# I'm looking for a predictive analysis method that can deal with the large number of 'NAs'
# I have in my basic TDF results dataset.

NA_model <- train(data = results_train, X2016_FP~., method = 'gbm', na.action = na.pass)
train_NA_model <- predict(NA_model, results_test, type = "raw")
# X2015_FP+X2014_FP+X2013_FP+X2012_FP+X2011_FP
View(train_NA_model)

# 'gbm' - gradient boosting model, 'lm' - linear regression, 'glm' - generalised linear regression, 'rf' - random forest
# 'C5.0' - , 'adaboost' - AdaBoost Classification Trees
# 'bayesglm' - Bayesian Generalized Linear Model
# 'ada' - Boosted Classification Trees
# 'glmboost' - Boosted Generalized Linear Model
# 'rpart' - 
# 'glmnet' - 
# 'kknn', 'knn' - k-Nearest Neighbors
# 
