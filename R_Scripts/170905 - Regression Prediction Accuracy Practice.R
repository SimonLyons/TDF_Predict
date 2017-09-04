
# My own sandpit practice on testing accuracy of models

# Create dataset
set.seed(1977)
x <- floor(runif(100, 0, 101))
my_error <- 1 * runif(100)
average_error <- mean(my_error)
y <- round((4 + (3 * x) + my_error),3)
my_data <- as.data.frame(cbind(x,y))

# Run regression analysis
library(caret)

lm_model <- train(data = my_data, y ~ x, method = "lm")
lm_model$finalModel
lm_model$results
plot(my_data$x, my_data$y, col = "blue")
abline(lm_model$finalModel$coefficients[1],lm_model$finalModel$coefficients[2], col = "red")

my_data$predict_lm <- predict(lm_model, my_data)
my_data$RMSE <- sqrt((my_data$predict_lm - my_data$y)^2)
mean(my_data$RMSE)
my_data$RMSED <- my_data$RMSE/mean(my_data$y)
View(my_data)
mean(my_data$RMSED)

lm_model2 <- lm(y ~ x, data = my_data)
lm_model2$residuals
crossprod(lm_model2$residuals)
