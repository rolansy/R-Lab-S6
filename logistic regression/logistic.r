#AIM: to implemnent simple & multinomial logistic regression in R.

library(ggplot2)   
library(dplyr)    
library(caret) 
library(nnet)     
data(iris)
cat("Head of the dataset:")
head(iris)
set.seed(123) 
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE, times = 1)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]
new_data <- data.frame(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2) #of class setosa

# simple logistic regression
simple_log_model <- glm(Species ~ ., data = trainData, family = "binomial")
predictions <- predict(simple_log_model, newdata = testData, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "virginica", "non-virginica")
mse <- mean((as.numeric(testData$Species == "virginica") - as.numeric(predicted_classes == "virginica"))^2)
cat("Mean Squared Error (MSE):", round(mse, 2))
predict_species <- function(model, new_data) {
  prediction <- predict(model, newdata = new_data, type = "response")
  ifelse(prediction > 0.5, "virginica", "non-virginica")
}
predicted_species <- predict_species(simple_log_model, new_data)
cat("Predicted species (Simple Logistic Regression):", predicted_species)

# multiple logistic regression
multi_log_model <- multinom(Species ~ ., data = trainData)
predictions <- predict(multi_log_model, newdata = testData, type = "class")
mse <- mean(predictions != testData$Species)
cat("Mean Squared Error (MSE):", round(mse, 2))
predict_species <- function(model, new_data) {
  predict(model, newdata = new_data, type = "class")
}
predicted_species <- predict_species(multi_log_model, new_data)
cat("Predicted species (Multinomial Logistic Regression):", predicted_species)

# Plotting the logistic regression results
# Plotting the logistic regression results
ggplot(trainData, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
    geom_point() +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    labs(title = "Logistic Regression - Petal Length vs Petal Width",
             x = "Petal Length",
             y = "Petal Width") +
    theme_minimal()

# Plotting the logistic regression curve
ggplot(trainData, aes(x = Petal.Length, y = as.numeric(Species == "virginica"))) +
    geom_point() +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    labs(title = "Logistic Regression Curve - Petal Length vs Probability of Virginica",
             x = "Petal Length",
             y = "Probability of Virginica") +
    theme_minimal()
