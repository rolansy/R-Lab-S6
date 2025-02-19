# Install and load necessary libraries
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("lattice")) install.packages("lattice", dependencies = TRUE)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", dependencies = TRUE)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr", dependencies = TRUE)

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data from the Excel file
file_path <- "stock_price.xlsx"
if (!file.exists(file_path)) {
  stop("File not found: ", file_path)
}

data <- read_excel(file_path)

# Check if the data was read correctly
if (is.null(data) || nrow(data) == 0) {
  stop("Failed to read data from the file or the file is empty.")
}

# Simple Linear Regression: Predicting Close price based on Open price
simple_model <- lm(Close ~ Open, data = data)
simple_model_summary <- summary(simple_model)
cat("Simple Linear Regression R-squared:", simple_model_summary$r.squared, "\n")

# Multiple Linear Regression: Predicting Close price based on Open, High, Low, and Volume
multiple_model <- lm(Close ~ Open + High + Low + Volume, data = data)
multiple_model_summary <- summary(multiple_model)
cat("Multiple Linear Regression R-squared:", multiple_model_summary$r.squared, "\n")

# Plot Simple Linear Regression using ggplot2
ggplot(data, aes(x = Open, y = Close)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Simple Linear Regression", x = "Open Price", y = "Close Price") +
    theme_minimal()

# Plot Simple Linear Regression using lattice
xyplot(Close ~ Open, data = data, type = c("p", "r"), auto.key = TRUE,
       main = "Simple Linear Regression (Lattice)", xlab = "Open Price", ylab = "Close Price")

# Plot Multiple Linear Regression using ggplot2 with facets
data_long <- data %>%
  pivot_longer(cols = c(Open, High, Low, Volume), names_to = "Predictor", values_to = "Value")

ggplot(data_long, aes(x = Value, y = Close)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    facet_wrap(~ Predictor, scales = "free_x") +
    labs(title = "Multiple Linear Regression", x = "Predictor Value", y = "Close Price") +
    theme_minimal()

# Plot Multiple Linear Regression using lattice with facets
xyplot(Close ~ Value | Predictor, data = data_long, type = c("p", "r"), auto.key = TRUE,
       main = "Multiple Linear Regression (Lattice)", xlab = "Predictor Value", ylab = "Close Price")

