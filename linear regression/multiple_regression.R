if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("lattice")) install.packages("lattice", dependencies = TRUE)

library(ggplot2)
library(lattice)

file_path <- "reg/city_day.csv"
air_quality <- read.csv(file_path)

air_quality <- na.omit(air_quality)
'''
str(air_quality)

sum(is.na(air_quality))

summary(air_quality)
'''
ggplot(air_quality, aes(x = PM2.5, y = AQI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "PM2.5", y = "AQI", title = "AQI vs PM2.5 (ggplot2)")

xyplot(AQI ~ PM2.5, data = air_quality,
       xlab = "PM2.5", ylab = "AQI",
       main = "AQI vs PM2.5 (lattice)",
       type = c("p", "r"))

simple_model <- lm(AQI ~ PM2.5, data = air_quality)

#summary(simple_model)

simple_predictions <- predict(simple_model, air_quality)

simple_rsquared <- cor(air_quality$AQI, simple_predictions)^2
cat("Simple Model R-squared:", simple_rsquared, "\n")

simple_rmse <- sqrt(mean((air_quality$AQI - simple_predictions)^2))
cat("Simple Model RMSE:", simple_rmse, "\n")

simple_mae <- mean(abs(air_quality$AQI - simple_predictions))
cat("Simple Model MAE:", simple_mae, "\n")

model <- lm(AQI ~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene + Toluene + Xylene, data = air_quality)

#summary(model)

predictions <- predict(model, air_quality)

rsquared <- cor(air_quality$AQI, predictions)^2
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

ggplot(air_quality_long, aes(x = Value, y = AQI)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  facet_wrap(~ Predictor, scales = "free_x") +
  labs(title = "Multiple Linear Regression", x = "Predictor Value", y = "AQI") +
  theme_minimal()

xyplot(AQI ~ Value | Predictor, data = air_quality_long, type = c("p", "r"), auto.key = TRUE,
       main = "Multiple Linear Regression (Lattice)", xlab = "Predictor Value", ylab = "AQI")

model <- lm(AQI ~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene + Toluene + Xylene, data = air_quality)

plot_data <- data.frame(AQI = air_quality$AQI, 
            PM2.5 = air_quality$PM2.5, 
            PM10 = air_quality$PM10, 
            NO = air_quality$NO, 
            NO2 = air_quality$NO2, 
            NOx = air_quality$NOx, 
            NH3 = air_quality$NH3, 
            CO = air_quality$CO, 
            SO2 = air_quality$SO2, 
            O3 = air_quality$O3, 
            Benzene = air_quality$Benzene, 
            Toluene = air_quality$Toluene, 
            Xylene = air_quality$Xylene)

library(reshape2)
plot_data_long <- melt(plot_data, id.vars = "AQI", variable.name = "Predictor", value.name = "Value")

ggplot(plot_data_long, aes(x = Value, y = AQI, color = Predictor)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Multiple Linear Regression", x = "Predictor Value", y = "AQI") +
  theme_minimal()

# Test sample values to predict AQI using the multiple model
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

model
sample_predictions <- predict(model, sample_values)
predict(model,sample_values)
cat("Predicted AQI for sample values:\n")
print(sample_predictions)

