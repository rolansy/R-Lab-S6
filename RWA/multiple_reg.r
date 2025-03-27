data1 <- read.csv("RWA/multiple_reg.csv")

# Build a multiple linear regression model
model <- lm(Weight ~ Height + Age, data = data1)
summary(model)  # Show model summary

# Take user input for prediction
a_height <- as.integer(readline("Enter the height of the individual: "))
a_age <- as.integer(readline("Enter the age of the individual: "))
a <- data.frame(Height = a_height, Age = a_age)

# Make prediction using the model
result <- predict(model, a)
cat("The predicted weight is", result, "kg\n")

# Plot the data (Height vs Weight)
plot(data1$Height, data1$Weight, col = "red", main = "Multiple Linear Regression", 
     cex = 1.3, pch = 16, xlab = "Height in cm", ylab = "Weight in Kg")
abline(model, col = "blue")


# Add the regression plane to the 3D scatter plot
s3d <- scatterplot3d(data1$Height, data1$Age, data1$Weight, pch = 16, color = "red", 
                          xlab = "Height", ylab = "Age", zlab = "Weight", main = "3D Regression Plot")

# Get the plane coefficients from the model
plane_coeff <- coef(model)

# Define the regression plane
s3d$plane3d(plane_coeff, draw_polygon = TRUE, polygon_args = list(col = rgb(0, 0, 1, 0.5)))