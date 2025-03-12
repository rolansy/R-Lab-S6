if (!require("rpart")) install.packages("rpart", dependencies = TRUE)
if (!require("rpart.plot")) install.packages("rpart.plot", dependencies = TRUE)

library(rpart)
library(rpart.plot)

file_path <- "decision tree/drug200.csv"
dataset <- read.csv(file_path)
dataset$Drug <- as.factor(dataset$Drug)

set.seed(123)
train_index <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

tree_model <- rpart(Drug ~ ., data = train_data, method = "class")

rpart.plot(tree_model, main = "Decision Tree for Drug Classification")

predictions <- predict(tree_model, test_data, type = "class")

accuracy <- sum(predictions == test_data$Drug) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

conf_matrix <- table(Predicted = predictions, Actual = test_data$Drug)
print("Confusion Matrix:")
print(conf_matrix)

predict_drug <- function() {
    cat("Enter the following details to predict the drug:\n")
    Age <- as.numeric(readline(prompt = "Age: "))
    Sex <- readline(prompt = "Sex (M/F): ")
    BP <- readline(prompt = "BP (HIGH/LOW/NORMAL): ")
    Cholesterol <- readline(prompt = "Cholesterol (HIGH/NORMAL): ")
    Na_to_K <- as.numeric(readline(prompt = "Na_to_K: "))
    user_input <- data.frame(Age = Age, Sex = Sex, BP = BP, Cholesterol = Cholesterol, Na_to_K = Na_to_K)
    prediction <- predict(tree_model, user_input, type = "class")
    cat("Predicted Drug:", as.character(prediction), "\n")
}

predict_drug()