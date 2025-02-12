library(ggplot2)
library(lattice)

data <- read.csv("regression_data.csv")

simple_model <- lm(y ~ x1, data = data)
summary(simple_model)

multiple_model <- lm(y ~ x1 + x2, data = data)
summary(multiple_model)

simple_model_summary <- summary(simple_model)
multiple_model_summary <- summary(multiple_model)

cat("Simple Linear Regression R-squared:", simple_model_summary$r.squared, "\n")
cat("Multiple Linear Regression R-squared:", multiple_model_summary$r.squared, "\n")

ggplot(data, aes(x = x1, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Simple Linear Regression", x = "X1", y = "Y") +
    theme_minimal()

xyplot(y ~ x1, data = data, type = c("p", "r"), auto.key = TRUE,
             main = "Simple Linear Regression (Lattice)", xlab = "X1", ylab = "Y")

ggplot(data, aes(x = x1, y = y, color = x2)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Multiple Linear Regression", x = "X1", y = "Y") +
    theme_minimal()

xyplot(y ~ x1 | factor(x2), data = data, type = c("p", "r"), auto.key = TRUE,
             main = "Multiple Linear Regression (Lattice)", xlab = "X1", ylab = "Y")
