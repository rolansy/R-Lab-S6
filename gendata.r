# Generate sample data
set.seed(123)
n <- 100
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- rnorm(n, mean = 30, sd = 5)
y <- 5 + 0.5 * x1 + 0.3 * x2 + rnorm(n, mean = 0, sd = 2)

# Create a data frame
data <- data.frame(x1 = x1, x2 = x2, y = y)

# Save the data to a CSV file
write.csv(data, "regression_data.csv", row.names = FALSE)