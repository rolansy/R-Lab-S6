library(nnet)
library(ggplot2)

data1<-read.csv("RWA/multinomial_logi.csv")
# Build a multinomial logistic regression model
model <- multinom(Purchase_Category ~ Age + Income, data = data1)
summary(model)  # Show model summary

# Take user input for prediction
a_age <- as.integer(readline("Enter the age of the individual: "))
a_income <- as.integer(readline("Enter the income of the individual: "))
a <- data.frame(Age = a_age, Income = a_income)

# Make prediction using the model
prob <- predict(model, a, type = "probs")
prediction <- predict(model, a)
cat("The predicted purchase category is", prediction, "\n")

# Plot the data (Age vs Purchase Category)
plot(data1$Age, as.numeric(data1$Purchase_Category), col = "red", main = "Multinomial Logistic Regression", 
     cex = 1.3, pch = 16, xlab = "Age in years", ylab = "Purchase Category (1=Low, 2=Medium, 3=High)")
