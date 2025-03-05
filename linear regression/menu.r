# Install and load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("lattice")) install.packages("lattice", dependencies = TRUE)
if (!require("reshape2")) install.packages("reshape2", dependencies = TRUE)

library(ggplot2)
library(lattice)
library(reshape2)

# Function to perform simple linear regression
simple_linear_regression <- function() {
  file_path <- "linear regression/Salary_dataset.csv"
  salary_data <- read.csv(file_path)
  
  ggplot(salary_data, aes(x = YearsExperience, y = Salary)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "Years of Experience", y = "Salary", title = "Salary vs Years of Experience (ggplot2)")
  
  xyplot(Salary ~ YearsExperience, data = salary_data,
         xlab = "Years of Experience", ylab = "Salary",
         main = "Salary vs Years of Experience (lattice)",
         type = c("p", "r"))
  
  simple_model <- lm(Salary ~ YearsExperience, data = salary_data)
  
  simple_predictions <- predict(simple_model, salary_data)
  
  simple_rsquared <- summary(simple_model)$r.squared
  cat("Simple Model R-squared:", simple_rsquared, "\n")
  
  simple_rmse <- sqrt(mean((salary_data$Salary - simple_predictions)^2))
  cat("Simple Model RMSE:", simple_rmse, "\n")
  
  simple_mae <- mean(abs(salary_data$Salary - simple_predictions))
  cat("Simple Model MAE:", simple_mae, "\n")
  
  simple_mape <- mean(abs((salary_data$Salary - simple_predictions) / salary_data$Salary)) * 100
  cat("Simple Model MAPE:", simple_mape, "%\n")
  
  user_input <- as.numeric(readline(prompt = "Enter years of experience: "))
  sample_years_experience <- data.frame(YearsExperience = user_input)
  predicted_salary <- predict(simple_model, sample_years_experience)
  cat("Predicted Salary for", user_input, "years of experience:", predicted_salary, "\n")
}

# Function to perform multiple linear regression
multiple_linear_regression <- function() {
  file_path <- "linear regression/city_day.csv"
  air_quality <- read.csv(file_path)
  
  air_quality <- na.omit(air_quality)
  
  model <- lm(AQI ~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene + Toluene + Xylene, data = air_quality)
  
  predictions <- predict(model, air_quality)
  
  rsquared <- summary(model)$r.squared
  cat("Multiple Model R-squared:", rsquared, "\n")
  
  rmse <- sqrt(mean((air_quality$AQI - predictions)^2))
  cat("Multiple Model RMSE:", rmse, "\n")
  
  mae <- mean(abs(air_quality$AQI - predictions))
  cat("Multiple Model MAE:", mae, "\n")
  
  air_quality_long <- reshape(air_quality, varying = list(c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene")), 
                              v.names = "Value", 
                              timevar = "Predictor", 
                              times = c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene"), 
                              direction = "long")
  
  ggplot(air_quality_long, aes(x = Value, y = AQI, color = Predictor)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Multiple Linear Regression", x = "Predictor Value", y = "AQI") +
    theme_minimal()
  
  xyplot(AQI ~ Value, groups = Predictor, data = air_quality_long, type = c("p", "r"), auto.key = list(columns = 3),
         main = "Multiple Linear Regression (Lattice)", xlab = "Predictor Value", ylab = "AQI")
  
  sample_values <- data.frame(
    PM2.5 = c(50, 80),
    PM10 = c(60, 90),
    NO = c(9, 30),
    NO2 = c(25, 35),
    NOx = c(45, 55),
    NH3 = c(12.2, 32.8),
    CO = c(1.9, 17.8),
    SO2 = c(45,69),
    O3 = c(33,55),
    Benzene = c(0.5, 2.7),
    Toluene = c(1, 21.5),
    Xylene = c(1.3, 6.4)
  )
  cat("Sample values:\n")
  print(sample_values)
  
  sample_predictions <- predict(model, sample_values)
  cat("Predicted AQI for sample values:\n")
  print(sample_predictions)
}

# Menu function
menu <- function() {
  cat("Menu:\n")
  cat("1. Perform Simple Linear Regression\n")
  cat("2. Perform Multiple Linear Regression\n")
  cat("3. Exit\n")
  choice <- as.integer(readline(prompt = "Enter your choice: "))
  return(choice)
}

# Main function
main <- function() {
  repeat {
    choice <- menu()
    if (choice == 1) {
      simple_linear_regression()
    } else if (choice == 2) {
      multiple_linear_regression()
    } else if (choice == 3) {
      cat("Exiting...\n")
      break
    } else {
      cat("Invalid choice! Please enter 1, 2, or 3.\n")
    }
  }
}

# Run the main function
main()