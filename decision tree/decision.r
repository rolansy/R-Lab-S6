# Load necessary libraries
if (!require("rpart")) install.packages("rpart", dependencies = TRUE)
if (!require("rpart.plot")) install.packages("rpart.plot", dependencies = TRUE)

library(rpart)
library(rpart.plot)

# Load the dataset
file_path <- "decision tree/drug200.csv"
dataset <- read.csv(file_path)
dataset$Drug <- as.factor(dataset$Drug)

# Split the dataset into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

# Function to calculate entropy
entropy <- function(target_col) {
    freq <- table(target_col) / length(target_col)
    -sum(freq * log2(freq))
}

# Function to calculate information gain
information_gain <- function(data, feature_col, target_col) {
    total_entropy <- entropy(data[[target_col]])
    values <- unique(data[[feature_col]])
    weighted_entropy <- sum(sapply(values, function(value) {
        subset <- data[data[[feature_col]] == value, ]
        (nrow(subset) / nrow(data)) * entropy(subset[[target_col]])
    }))
    total_entropy - weighted_entropy
}

# Function to build the ID3 decision tree
id3 <- function(data, target_col, features) {
    unique_targets <- unique(data[[target_col]])
    
    # If all target values are the same, return that value
    if (length(unique_targets) == 1) {
        return(unique_targets[1])
    }
    
    # If no features left, return the most common target value
    if (length(features) == 0) {
        return(names(sort(table(data[[target_col]]), decreasing = TRUE))[1])
    }
    
    # Select the feature with the highest information gain
    gains <- sapply(features, function(feature) information_gain(data, feature, target_col))
    best_feature <- features[which.max(gains)]
    
    # Create the tree structure
    tree <- list()
    tree[[best_feature]] <- list()
    
    # Split the data and recursively build the tree
    for (value in unique(data[[best_feature]])) {
        subset <- data[data[[best_feature]] == value, ]
        subtree <- id3(subset, target_col, setdiff(features, best_feature))
        tree[[best_feature]][[value]] <- subtree
    }
    
    return(tree)
}

# Build the ID3 decision tree
features <- setdiff(names(train_data), "Drug")
id3_tree <- id3(train_data, "Drug", features)

# Function to predict using the ID3 tree
predict_id3 <- function(tree, sample) {
    if (!is.list(tree)) {
        return(tree)
    }
    feature <- names(tree)[1]
    value <- sample[[feature]]
    subtree <- tree[[feature]][[value]]
    return(predict_id3(subtree, sample))
}

# Predict on the test data
predictions <- sapply(1:nrow(test_data), function(i) predict_id3(id3_tree, test_data[i, ]))

# Calculate accuracy
accuracy <- sum(predictions == test_data$Drug) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

# Confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test_data$Drug)
print("Confusion Matrix:")
print(conf_matrix)

# Function to predict drug based on user input
predict_drug <- function() {
        cat("Enter the following details to predict the drug:\n")
        Age <- as.numeric(readline(prompt = "Age: "))
        Sex <- readline(prompt = "Sex (M/F): ")
        BP <- readline(prompt = "BP (HIGH/LOW/NORMAL): ")
        Cholesterol <- readline(prompt = "Cholesterol (HIGH/NORMAL): ")
        Na_to_K <- as.numeric(readline(prompt = "Na_to_K: "))
        user_input <- data.frame(Age = Age, Sex = Sex, BP = BP, Cholesterol = Cholesterol, Na_to_K = Na_to_K)
        prediction <- predict_id3(id3_tree, user_input)
        cat("Predicted Drug:", as.character(prediction), "\n")
}

predict_drug()
