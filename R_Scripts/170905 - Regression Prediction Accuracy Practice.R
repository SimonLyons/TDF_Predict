
# My own sandpit practice on testing accuracy of models

# Create dataset
set.seed(1977)
x <- floor(runif(100, 0, 101))
my_error <- 10 * runif(100)
average_error <- mean(my_error)
y <- round((4 + (3 * x) + my_error),3)
my_data <- as.data.frame(cbind(x,y))

# Run regression analysis
require(caret)

# Calculate/determine logistic regression model
lm_model <- train(data = my_data, y ~ x, method = "lm")
lm_model$finalModel
lm_model$results

# Plot results
plot(my_data$x, my_data$y, col = "blue")   # Plot data points
abline(lm_model$finalModel$coefficients[1],lm_model$finalModel$coefficients[2], col = "red")   # Plot predictive model line

my_data$predict_lm <- predict(lm_model, my_data)
my_data$RMSE <- (my_data$y - my_data$predict_lm)^2
mean(my_data$RMSE)
RMSE_calc <- sqrt(mean(my_data$RMSE))
lm_model$results$Rsquared
lm_model$results$RMSE
RMSE_mean <- sum(sqrt(my_data$RMSE))/nrow(my_data)
NRMSE <- RMSE_calc/mean(my_data$y)

mean(my_data$RMSE)
my_data$RMSED <- my_data$RMSE/mean(my_data$y)
View(my_data)
mean(my_data$RMSED)

lm_model2 <- lm(y ~ x, data = my_data)
lm_model2$residuals
crossprod(lm_model2$residuals)


###############################################################
# Run a loop that increases error, calculated a model, predicts 
# results and then determines the accuracy for comparison.

# Set error values
error_values <- c(1, 10, 100, 1000)
RMSE <- rep(NA, 4)
RMSED <- rep(NA, 4)
# Create empty dataframe for populating with error results
error_df <- as.data.frame(matrix(data = NA, nrow = 4, ncol = 6))
my_names <- rep(c("RMSE", "RMSED"), 3)
my_models <- c(rep("lm",2), rep("glm", 2), rep("rf",2))
colnames(error_df) <- paste(my_names, my_models, sep = "_")
error_df <- cbind(error_values, error_df)

for (e in 1:length(error_values)){
  # Create dataset
  set.seed(1977)
  x <- floor(runif(100, 0, 101))
  my_error <- error_values[e] * runif(100)
  y <- round((4 + (3 * x) + my_error),3)
  my_data <- as.data.frame(cbind(x,y))
  
  # Load required packages
  require(caret)
  # Calculate/determine logistic regression model
  lm_model <- train(data = my_data, y ~ x, method = "lm")
  glm_model <- train(data = my_data, y ~ x, method = "glm")
  rf_model <- train(data = my_data, y ~ x, method = "rf")
  
  # Predict results
  # my_data$predict_lm <- predict(lm_model, my_data)
  # my_data$delta_squared <- (my_data$y - my_data$predict_lm)^2
  
  # Store error RMSE in 
  error_df[e, 2] <- lm_model$results$RMSE
  error_df[e, 3] <- lm_model$results$RMSE / mean(my_data$y)
  error_df[e, 4] <- glm_model$results$RMSE
  error_df[e, 5] <- glm_model$results$RMSE / mean(my_data$y)
  error_df[e, 6] <- rf_model$results$RMSE
  error_df[e, 7] <- rf_model$results$RMSE / mean(my_data$y)
}

View(error_df)

