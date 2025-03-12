# Load necessary libraries
library(rpart)
library(rpart.plot)
library(caret)

# Load the dataset
data <- read.csv("/home/ds-da-14/Ronal/R-Lab-S6/decision tree/PlayTennis.csv")

# Convert categorical variables to factors
data$outlook <- as.factor(data$outlook)
data$temp <- as.factor(data$temp)
data$humidity <- as.factor(data$humidity)
data$windy <- as.factor(data$windy)
data$play <- as.factor(data$play)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$play, p = 0.8, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Train the ID3 decision tree model
model <- rpart(play ~ ., data = trainData, method = "class", parms = list(split = "information"))

# Display the decision tree with enhanced visualization
rpart.plot(model, type = 3, extra = 101, fallen.leaves = TRUE, main = "Decision Tree for PlayTennis")

# Predict on the test data
predictions <- predict(model, testData, type = "class")

# Show model accuracy
confusionMatrix(predictions, testData$play)

# Function to predict play outcome based on input values
predict_play <- function(outlook, temp, humidity, windy) {
    new_data <- data.frame(outlook = factor(outlook, levels = levels(data$outlook)),
                                                 temp = factor(temp, levels = levels(data$temp)),
                                                 humidity = factor(humidity, levels = levels(data$humidity)),
                                                 windy = factor(windy, levels = levels(data$windy)))
    prediction <- predict(model, new_data, type = "class")
    return(prediction)
}

# Take inputs from user
outlook <- readline(prompt = "Enter outlook (sunny, overcast, rainy): ")
temp <- readline(prompt = "Enter temperature (hot, mild, cool): ")
humidity <- readline(prompt = "Enter humidity (high, normal): ")
windy <- as.logical(readline(prompt = "Is it windy (TRUE, FALSE): "))

# Predict based on user input
user_prediction <- predict_play(outlook, temp, humidity, windy)
print(paste("Prediction for outlook=", outlook, ", temp=", temp, ", humidity=", humidity, ", windy=", windy, ": ", user_prediction, sep=""))