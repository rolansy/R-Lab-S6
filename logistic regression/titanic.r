library(caTools)
library(ggplot2)

# Load the dataset
data <- read.csv("logistic regression/train.csv")

# Check for missing values
sum(is.na(data))

# Remove rows with missing values
data <- na.omit(data)

# Convert categorical variables to factors
data$Sex <- factor(data$Sex)
data$Ticket <- factor(data$Ticket)
data$Cabin <- factor(data$Cabin)

# Split data into training and testing sets
set.seed(123) # for reproducibility
split <- sample.split(data$Survived, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Train the logistic regression model
model <- glm(Survived ~ Pclass + Sex + Age + Fare,
             data = train_data, family = binomial)

# Summary of the logistic regression model
summary(model)

# Prediction on test data
predictions <- predict(model, newdata = test_data, type = "response")
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(binary_predictions == test_data$Survived)
print(paste("Accuracy:", accuracy * 100))

# Plot sigmoid curve
x <- seq(-10, 10, length.out = 100)
sigmoid <- 1 / (1 + exp(-x))

# Plot data points and sigmoid curve using ggplot2
ggplot() +
  geom_point(data = data, aes(x = Age, y = Survived, color = factor(Survived))) +
  labs(title = "Relationship between Age and Survival Status",
       x = "Age",
       y = "Survived",
       color = "Survived") +
  theme_minimal() +
  geom_line(data = data.frame(x = x, sigmoid = sigmoid), aes(x = x, y = sigmoid),
            color = "blue", linetype = "solid") +
  labs(title = "Relationship between Age and Survival Status with Sigmoid Curve",
       x = "Age",
       y = "Probability")

# Take user input
user_Pclass <- as.numeric(readline(prompt = "Enter Pclass (1, 2, or 3): "))
user_Sex <- as.factor(readline(prompt = "Enter Sex (male or female): "))
user_Age <- as.numeric(readline(prompt = "Enter Age: "))
user_Fare <- as.numeric(readline(prompt = "Enter Fare: "))

# Create a new data frame with user input
user_data <- data.frame(Pclass = user_Pclass, Sex = user_Sex, Age = user_Age, Fare = user_Fare)

# Predict survival probability for user input
user_prediction <- predict(model, newdata = user_data, type = "response")
user_binary_prediction <- ifelse(user_prediction > 0.5, 1, 0)

# Display prediction result
print(paste("Predicted Survival Probability:", user_prediction))
print(paste("Predicted Survival (0 = No, 1 = Yes):", user_binary_prediction))
