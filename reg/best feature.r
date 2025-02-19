# Load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("lattice")) install.packages("lattice", dependencies = TRUE)

library(ggplot2)
library(lattice)

# Load the dataset
file_path <- "reg/city_day.csv"
air_quality <- read.csv(file_path)

# Remove rows with missing values
air_quality <- na.omit(air_quality)

# List of features to test
features <- c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene")

# Initialize a data frame to store the results
results <- data.frame(Feature = character(), R_squared = numeric(), RMSE = numeric(), MAE = numeric(), stringsAsFactors = FALSE)

# Loop through each feature and perform simple linear regression
for (feature in features) {
  # Perform simple linear regression
  formula <- as.formula(paste("AQI ~", feature))
  model <- lm(formula, data = air_quality)
  
  # Predict AQI using the model
  predictions <- predict(model, air_quality)
  
  # Calculate performance metrics
  r_squared <- summary(model)$r.squared
  rmse <- sqrt(mean((air_quality$AQI - predictions)^2))
  mae <- mean(abs(air_quality$AQI - predictions))
  
  # Store the results
  results <- rbind(results, data.frame(Feature = feature, R_squared = r_squared, RMSE = rmse, MAE = mae))
}

# Print the results
print(results)

# Find the feature with the least RMSE
best_feature <- results[which.min(results$RMSE), ]
cat("Feature with the least RMSE:\n")
print(best_feature)