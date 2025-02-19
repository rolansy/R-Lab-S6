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

# Check the structure of the dataset
str(air_quality)

# Check for missing values
sum(is.na(air_quality))

# Summary statistics
summary(air_quality)

# Plot AQI vs other variables
# Using ggplot2 for PM2.5
ggplot(air_quality, aes(x = PM2.5, y = AQI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "PM2.5", y = "AQI", title = "AQI vs PM2.5 (ggplot2)")

# Using lattice for PM2.5
xyplot(AQI ~ PM2.5, data = air_quality,
       xlab = "PM2.5", ylab = "AQI",
       main = "AQI vs PM2.5 (lattice)",
       type = c("p", "r"))

# Perform multiple linear regression
model <- lm(AQI ~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene + Toluene + Xylene, data = air_quality)

# Summary of the model
summary(model)

# Predict AQI using the model
predictions <- predict(model, air_quality)

# Evaluate the performance of the model
# Calculate R-squared
rsquared <- cor(air_quality$AQI, predictions)^2
cat("R-squared:", rsquared, "\n")

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((air_quality$AQI - predictions)^2))
cat("RMSE:", rmse, "\n")

# Calculate MAE (Mean Absolute Error)
mae <- mean(abs(air_quality$AQI - predictions))
cat("MAE:", mae, "\n")

# Reshape data for faceting without using tidyverse
air_quality_long <- reshape(air_quality, varying = list(c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene")), 
                            v.names = "Value", 
                            timevar = "Predictor", 
                            times = c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene"), 
                            direction = "long")

# Plot Multiple Linear Regression using ggplot2 with facets
ggplot(air_quality_long, aes(x = Value, y = AQI)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  facet_wrap(~ Predictor, scales = "free_x") +
  labs(title = "Multiple Linear Regression", x = "Predictor Value", y = "AQI") +
  theme_minimal()

# Plot Multiple Linear Regression using lattice with facets
xyplot(AQI ~ Value | Predictor, data = air_quality_long, type = c("p", "r"), auto.key = TRUE,
       main = "Multiple Linear Regression (Lattice)", xlab = "Predictor Value", ylab = "AQI")


