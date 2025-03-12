# Load necessary libraries
#if (!require("DiagrammeR")) install.packages("DiagrammeR", dependencies = TRUE)
library(DiagrammeR)

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
    if (all(is.na(gains))) {
        return(names(sort(table(data[[target_col]]), decreasing = TRUE))[1])
    }
    best_feature <- features[which.max(gains)]
    
    # Create the tree structure
    tree <- list()
    tree[[best_feature]] <- list()
    
    # Split the data and build subtrees
    feature_values <- unique(data[[best_feature]])
    for (value in feature_values) {
        subset <- data[data[[best_feature]] == value, ]
        subtree <- id3(subset, target_col, setdiff(features, best_feature))
        tree[[best_feature]][[value]] <- subtree
    }
    
    return(tree)
}

# Function to print the decision tree
print_tree <- function(tree, indent = "") {
    if (is.character(tree)) {
        cat(indent, "->", tree, "\n")
    } else {
        for (feature in names(tree)) {
            for (value in names(tree[[feature]])) {
                cat(indent, feature, "=", value, "\n")
                print_tree(tree[[feature]][[value]], paste(indent, "  "))
            }
        }
    }
}

# Function to convert the decision tree to Graphviz format for visualization
convert_to_graphviz <- function(tree, node_id = 1, parent_id = NULL, edge_label = NULL) {
    nodes <- list()
    edges <- list()
    
    if (is.character(tree)) {
        nodes[[node_id]] <- list(label = tree, shape = "box")
    } else {
        feature <- names(tree)[1]
        nodes[[node_id]] <- list(label = feature)
        for (value in names(tree[[feature]])) {
            child_id <- max(as.numeric(names(nodes))) + 1
            child_nodes_edges <- convert_to_graphviz(tree[[feature]][[value]], child_id, node_id, value)
            nodes <- c(nodes, child_nodes_edges$nodes)
            edges <- c(edges, child_nodes_edges$edges)
        }
    }
    
    if (!is.null(parent_id)) {
        edges[[length(edges) + 1]] <- list(from = parent_id, to = node_id, label = edge_label)
    }
    
    return(list(nodes = nodes, edges = edges))
}

# Function to plot the decision tree using DiagrammeR
plot_tree <- function(tree) {
    graphviz_data <- convert_to_graphviz(tree)
    nodes <- graphviz_data$nodes
    edges <- graphviz_data$edges
    
    node_statements <- sapply(names(nodes), function(id) {
        node <- nodes[[id]]
        paste0(id, " [label = \"", node$label, "\", shape = \"", node$shape, "\"];")
    })
    
    edge_statements <- sapply(edges, function(edge) {
        paste0(edge$from, " -> ", edge$to, " [label = \"", edge$label, "\"];")
    })
    
    graphviz_code <- paste0("digraph G {\n", paste(node_statements, collapse = "\n"), "\n", paste(edge_statements, collapse = "\n"), "\n}")
    grViz(graphviz_code)
}

# Build the decision tree using the ID3 algorithm
features <- colnames(train_data)[colnames(train_data) != "Drug"]
decision_tree <- id3(train_data, "Drug", features)

# Print the decision tree
cat("Decision Tree:\n")
print_tree(decision_tree)

# Plot the decision tree
plot_tree(decision_tree)

# Function to predict the drug class based on the decision tree
predict_drug <- function(tree, input_data) {
    if (is.character(tree)) {
        return(tree)
    } else {
        feature <- names(tree)[1]
        value <- input_data[[feature]]
        if (!is.null(tree[[feature]][[value]])) {
            return(predict_drug(tree[[feature]][[value]], input_data))
        } else {
            return(NA)
        }
    }
}

# Function to take user inputs and predict the drug
predict_drug_from_input <- function(tree) {
    cat("Enter the following details to predict the drug:\n")
    Age <- as.numeric(readline(prompt = "Age: "))
    Sex <- readline(prompt = "Sex (M/F): ")
    BP <- readline(prompt = "BP (HIGH/LOW/NORMAL): ")
    Cholesterol <- readline(prompt = "Cholesterol (HIGH/NORMAL): ")
    Na_to_K <- as.numeric(readline(prompt = "Na_to_K: "))
    
    user_input <- data.frame(Age = Age, Sex = Sex, BP = BP, Cholesterol = Cholesterol, Na_to_K = Na_to_K)
    prediction <- predict_drug(tree, user_input)
    cat("Predicted Drug:", as.character(prediction), "\n")
}

# Call the function to predict drug based on user inputs
predict_drug_from_input(decision_tree)

# Evaluate the model on the test data
test_predictions <- sapply(1:nrow(test_data), function(i) predict_drug(decision_tree, test_data[i, ]))
accuracy <- sum(test_predictions == test_data$Drug) / nrow(test_data)
cat("Accuracy:", accuracy, "\n")

# Confusion matrix
conf_matrix <- table(Predicted = test_predictions, Actual = test_data$Drug)
print("Confusion Matrix:")
print(conf_matrix)