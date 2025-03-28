# Load necessary libraries
library(caTools)
library(e1071)
library(ggplot2)
library(C50)

# Function for Program 1 (SVM)
run_svm_program <- function() {
  # Load dataset
  dataset = read.csv('~/Ronal/R-Lab-S6/decision tree/svm.csv')
  
  # Selecting relevant columns
  dataset = dataset[, 3:5]  # Keeping only Age, EstimatedSalary, and Purchased
  
  # Encoding the target variable as a factor
  dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
  
  # Splitting dataset into training and test sets
  set.seed(123)
  split = sample.split(dataset$Purchased, SplitRatio = 0.75)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  # Feature Scaling
  training_set[, 1:2] = scale(training_set[, 1:2])
  test_set[, 1:2] = scale(test_set[, 1:2])
  
  # Fitting SVM with RBF Kernel (Non-Linear)
  classifier = svm(formula = Purchased ~ ., 
                   data = training_set, 
                   type = 'C-classification', 
                   kernel = 'radial')
  
  # Predicting the Test set results
  y_pred = predict(classifier, newdata = test_set[, -3])
  
  # Confusion Matrix
  cm = table(test_set[, 3], y_pred)
  print("Confusion Matrix : ")
  print(cm)
  
  # Function to visualize decision boundary
  plot_decision_boundary <- function(set, title) {
    X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
    X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
    
    grid_set = expand.grid(X1, X2)
    colnames(grid_set) = c('Age', 'EstimatedSalary')
    y_grid = predict(classifier, newdata = grid_set)
    
    plot(set[, -3], 
         main = title, 
         xlab = 'Age', ylab = 'Estimated Salary', 
         xlim = range(X1), ylim = range(X2), 
         col = ifelse(set[, 3] == 1, 'darkgreen', 'darkred'), 
         pch = 19, cex = 1.2)
    
    contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), 
        add = TRUE, drawlabels = TRUE, lwd = 2, col = "blue")
    points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'lightgreen', 'lightpink'))
    
    points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'), cex = 1)
  }
  
  # Visualizing the Training set results
  plot_decision_boundary(training_set, "SVM with RBF Kernel (Training set)")
  
  # Visualizing the Test set results
  plot_decision_boundary(test_set, "SVM with RBF Kernel (Test set)")
  
  # User Input for Prediction
  pred1 <- function() {
    # Get user input for Age and Estimated Salary
    new_age = as.numeric(readline(prompt = "Enter Age : "))
    new_salary = as.numeric(readline(prompt = "Enter Salary : "))
    
    # Handle invalid input
    if (is.na(new_age) | is.na(new_salary)) {
      stop("Invalid input! Please enter numeric values.")
    }
    
    # Scale the new input using the same scaling as training data
    new_data = data.frame(Age = (new_age - mean(dataset$Age)) / sd(dataset$Age),
                          EstimatedSalary = (new_salary - mean(dataset$EstimatedSalary)) / sd(dataset$EstimatedSalary))
    
    # Predict class for new input
    prediction = predict(classifier, newdata = new_data)
    print(prediction)
    
    # Display result
    if (prediction == 1) {
      cat("\nPrediction Result:\n The person is likely to Purchase.\n")
    } else {
      cat("\nPrediction Result:\n The person is NOT likely to Purchase.\n")
    }
  }
  
  # Call the function
  pred1()
}

# Function for Program 2 (Decision Tree)
run_decision_tree_program <- function() {
  # Load the dataset
  play_tennis_data <- read.csv("~/Ronal/R-Lab-S6/decision tree/tennis.csv")
  
  # Convert categorical columns to factors
  play_tennis_data$Outlook <- factor(play_tennis_data$Outlook, levels = c("Sunny", "Overcast", "Rain"))
  play_tennis_data$Temperature <- factor(play_tennis_data$Temperature, levels = c("Hot", "Mild", "Cool"))
  play_tennis_data$Humidity <- factor(play_tennis_data$Humidity, levels = c("High", "Normal"))
  play_tennis_data$Wind <- factor(play_tennis_data$Wind, levels = c("Weak", "Strong"))
  play_tennis_data$PlayTennis <- factor(play_tennis_data$PlayTennis, levels = c("No", "Yes"))
  
  # Train a C5.0 decision tree model
  model <- C5.0(PlayTennis ~ Outlook + Temperature + Humidity + Wind, data = play_tennis_data)
  
  # Function for custom user input
  get_user_input <- function() {
    cat("Enter Options for the following :\n")
    
    outlook <- readline(prompt = "Enter Outlook (Sunny, Overcast, Rain): ")
    temperature <- readline(prompt = "Enter Temperature (Hot, Mild, Cool): ")
    humidity <- readline(prompt = "Enter Humidity (High, Normal): ")
    wind <- readline(prompt = "Enter Wind (Weak, Strong): ")
    
    return(data.frame(
      Outlook = factor(outlook, levels = c("Sunny", "Overcast", "Rain")),
      Temperature = factor(temperature, levels = c("Hot", "Mild", "Cool")),
      Humidity = factor(humidity, levels = c("High", "Normal")),
      Wind = factor(wind, levels = c("Weak", "Strong"))
    ))
  }
  
  # Visualize the decision tree using C5.0's built-in plot function
  library(partykit)
  tree <- as.party(model)
  plot(tree, main = "Decision Tree Visualization", type = "simple", gp = gpar(fontsize = 10, col = "blue"))
  
  test_data <- get_user_input()
  prediction <- predict(model, test_data)
  cat(paste("Predicted Class:", prediction, "\n"))
  
  # Display result based on prediction
  if (prediction == "Yes") {
    cat("\nPrediction Result:\n The person is likely to Play Tennis.\n")
  } else {
    cat("\nPrediction Result:\n The person is NOT likely to Play Tennis.\n")
  }
}

# Main Menu
main_menu <- function() {
  while (TRUE) {
    cat("\nMain Menu\n")
    cat("1. SVM\n")
    cat("2. Decision Tree\n")
    cat("3. Exit\n")
    choice <- as.numeric(readline(prompt = "Enter your choice : "))
    
    if (choice == 1) {
      run_svm_program()
    } else if (choice == 2) {
      run_decision_tree_program()
    } else if (choice == 3) {
      cat("Exiting...\n")
      break
    } else {
      cat("Invalid choice! Please try again.\n")
    }
  }
}

# Run the main menu
main_menu()
