
library(nnet)
binomial_data <- read.csv("logistic regression/binomial.csv")
binomial_model <- glm(Purchase ~ Salary+Age, data = binomial_data, family = binomial(link = "logit"))

iris_data <- read.csv("logistic regression/multinomial.csv")

iris_data$species <- as.factor(iris_data$species)
iris_data$species <- relevel(iris_data$species, ref = "setosa")

multinomial_model <- multinom(species ~ sepal_length + sepal_width + petal_length + petal_width, data = iris_data)

menu <- function() {
  repeat {
    ch <- readline(prompt = "Choose the type of logistic regression:\n1. Binomial\n2. Multinomial\n3. Exit\nEnter your choice: ")
    
    if (ch == "1") {
      print("Binomial Logistic Regression...")
      #print(summary(binomial_model))
      age_input <- as.numeric(readline(prompt="Enter Age: "))
      salary_input <- as.numeric(readline(prompt = "Enter Salary: "))
      new_data <- data.frame(Age = age_input, Salary = salary_input)
      predicted_prob <- predict(binomial_model, newdata = new_data, type = "response")
      print(paste("Predicted Probability of Purchase:", round(predicted_prob)))
      
      # Predict on training data
      binomial_predictions <- predict(binomial_model, newdata = binomial_data, type = "response")
      binomial_predicted_class <- ifelse(binomial_predictions > 0.5, 1, 0)
      
      # Confusion Matrix
      binomial_cm <- table(Predicted = binomial_predicted_class, Actual = binomial_data$Purchase)
      print("Confusion Matrix for Binomial Logistic Regression:")
      print(binomial_cm)
      
      # Accuracy Calculation
      binomial_accuracy <- sum(diag(binomial_cm)) / sum(binomial_cm)
      print(paste("Binomial Model Accuracy:", round(binomial_accuracy, 4)))
      
    } else if (ch == "2") {
      print("Multinomial Logistic Regression...")
      #print(summary(multinomial_model))
      
      sl_input <- as.numeric(readline(prompt = "Enter Sepal length: "))
      sw_input <- as.numeric(readline(prompt = "Enter Sepal Width: "))
      pl_input <- as.numeric(readline(prompt = "Enter Petal Length: "))
      pw_input <- as.numeric(readline(prompt = "Enter Petal Width: "))
      
      new_data <- data.frame(
        sepal_length = sl_input,
        sepal_width = sw_input,
        petal_length = pl_input,
        petal_width = pw_input
      )
      
      #softmax_probs <- apply(predicted_probs, 1, softmax)
      #print("Softmax Probabilities:")
      #print(softmax_probs)
      
      #predicted_probs <- predict(multinomial_model, newdata = new_data, type = "probs")
      #print("Predicted Probabilities:")
      #print(predicted_probs)
      
      predicted_class <- predict(multinomial_model, newdata = new_data, type = "class")
      print(paste("Predicted Iris Species:", predicted_class))
      
      # Predict on training data
      multinomial_predicted_class <- predict(multinomial_model, newdata = iris_data, type = "class")
      
      # Confusion Matrix
      multinomial_cm <- table(Predicted = multinomial_predicted_class, Actual = iris_data$species)
      print("Confusion Matrix for Multinomial Logistic Regression:")
      print(multinomial_cm)
      
      multinomial_accuracy <- sum(diag(multinomial_cm)) / sum(multinomial_cm)
      print(paste("Multinomial Model Accuracy:", round(multinomial_accuracy, 4)))
      
      
    } else if (ch == "3") {
      print("Exiting")
      break
      
    } else {
      print("Invalid choice")
    }
  }
}

menu()

