if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("lattice")) install.packages("lattice", dependencies = TRUE)

library(ggplot2)
library(lattice)

file_path <- "reg/Salary_dataset.csv"
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

#summary(simple_model)

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
