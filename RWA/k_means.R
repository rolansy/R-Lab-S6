library(ggplot2)

# Load Data
data <- data.frame(
  X = c(1.0, 1.5, 3.0, 5.0, 3.5, 4.5, 3.5),
  Y = c(1.0, 2.0, 4.0, 7.0, 5.0, 5.0, 4.5)
)

# Function to calculate Euclidean distance
calculate_distance <- function(p1, p2) {
  return(sqrt(sum((p1 - p2) ^ 2)))
}

# K-Means Clustering Algorithm
k_means <- function(data, k, max_iterations = 3) {
  set.seed(123)
  
  # Initialize cluster assignments
  clusters <- rep(0, nrow(data))
  
  # Get initial centroids from user
  centroids <- matrix(nrow = k, ncol = 2)
  for (i in 1:k) {
    cat("Enter coordinates for centroid", i, "(X Y): ")
    centroids[i, ] <- scan(nmax = 2)
  }
  
  # Iterate to update clusters
  for (iteration in 1:max_iterations) {
    cat("\nIteration:", iteration, "\n")
    
    # Assign points to closest centroid
    for (i in 1:nrow(data)) {
      distances <- numeric(k)
      for (j in 1:k) {
        distances[j] <- calculate_distance(as.numeric(data[i, ]), centroids[j, ])
      }
      clusters[i] <- which.min(distances)
    }
    
    # Calculate new centroids
    new_centroids <- centroids  # Copy current centroids
    
    # Update each centroid by finding the average of points in that cluster
    for (j in 1:k) {
      cluster_points <- data[clusters == j, ]  # Get all points in cluster j
      
      if (nrow(cluster_points) > 0) {  # If cluster is not empty
        new_centroids[j, ] <- colMeans(cluster_points)  # Update centroid as mean of points
      }
    }
    
    
    # Print updated centroids
    cat("\nUpdated Centroids:\n")
    print(new_centroids)
    
    # Check for convergence
    if (all(centroids == new_centroids)) {
      cat("\nConverged at iteration:", iteration, "\n")
      break
    }
    
    centroids <- new_centroids
  }
  
  return(list(centroids = centroids, clusters = clusters))
}

# User input for number of clusters
cat("Enter the number of clusters (k): ")
k <- as.integer(readline())

# Run K-Means
result <- k_means(data, k)

# Plot Results
plot(data$X, data$Y, col = result$clusters, pch = 16, main = "K-Means Clustering", xlab = "X", ylab = "Y")
points(result$centroids[, 1], result$centroids[, 2], col = 1:k, pch = 8, cex = 2)
