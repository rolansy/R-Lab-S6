library(ggplot2)


# Read data from CSV file
data1 <- read.csv("binomial_logi.csv")

# Build a binary logistic regression model
model <- glm(Obese ~ Height + Age, data = data1, family = binomial)
summary(model)  # Show model summary

# Take user input for prediction
a_height <- as.integer(readline("Enter the height of the individual: "))
a_age <- as.integer(readline("Enter the age of the individual: "))
a <- data.frame(Height = a_height, Age = a_age)

# Make prediction using the model
prob <- predict(model, a, type = "response")
prediction <- ifelse(prob > 0.5, 1, 0)
cat("The predicted obesity status is", prediction, "(1 = Obese, 0 = Not Obese)\n")

# Plot the data (Height vs Obese)
plot(data1$Height, data1$Obese, col = "red", main = "Binary Logistic Regression", 
     cex = 1.3, pch = 16, xlab = "Height in cm", ylab = "Obese (1 = Yes, 0 = No)")
