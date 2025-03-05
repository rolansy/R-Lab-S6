# AIM: to predict the air quality using linear regression model. Analyse its performance & plot the values using GGplot & lattice plot.

library(ggplot2)
library(lattice)
library(caret)
air_quality <- read.csv("linear regression/city_day.csv",header = TRUE)
air_quality <- na.omit(air_quality)
ggplot(air_quality, aes(x = PM2.5, y = AQI)) +geom_point() +geom_smooth(method = "lm") +labs(x = "PM2.5", y = "AQI", title = "AQI vs PM2.5 (ggplot2)")
xyplot(AQI ~ PM2.5, data = air_quality, xlab = "PM2.5", ylab = "AQI",main = "AQI vs PM2.5 (lattice plot)",type = c("p", "r"))
set.seed(123)  
train_index <- createDataPartition(air_quality$AQI, p = 0.7, list = FALSE) # 70% training, 30% test dataset
train_data <- air_quality[train_index, ]
test_data <- air_quality[-train_index, ]
model <- lm(AQI ~ PM2.5 + NO + NO2 + NOx + CO + SO2 + O3 + Benzene + Toluene + Xylene, data = train_data) #multiple linear regression model is used
summary(model)
new_data <- data.frame(PM2.5 = 53.07,NO = 4.17,NO2 = 13.82,NOx = 17.45,CO = 4.17,SO2 = 21.76,O3 = 4.47,Benzene = 6.94,Toluene = 0.42,Xylene = 0.69)
prediction <- predict(model, newdata = new_data) #actual prediction = 164
cat("Predicted AQI:", prediction, "\n")
predictions <- predict(model, newdata = test_data)
rmse <- sqrt(mean((test_data$AQI - predictions)^2))
cat("RMSE:", rmse, "\n")

'''
_____________________________________________________
input: [city_day.csv]
_____________________________________________________
output:
Call:
lm(formula = AQI ~ PM2.5 + NO + NO2 + NOx + CO + SO2 + O3 + Benzene + 
    Toluene + Xylene, data = train_data)

Residuals:
    Min      1Q  Median      3Q     Max 
-301.38  -17.23   -5.01   11.75  386.32 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) 20.9139736  1.3306840  15.717  < 2e-16 ***
PM2.5        1.2832758  0.0131582  97.527  < 2e-16 ***
NO          -0.0073113  0.0525716  -0.139    0.889    
NO2          0.0002153  0.0427983   0.005    0.996    
NOx          0.5466939  0.0480400  11.380  < 2e-16 ***
CO           8.7003826  0.4209250  20.670  < 2e-16 ***
SO2          0.0676124  0.0782920   0.864    0.388    
O3           0.4572883  0.0275975  16.570  < 2e-16 ***
Benzene     -0.5342339  0.1226588  -4.355 1.36e-05 ***
Toluene     -0.0093617  0.0542751  -0.172    0.863    
Xylene      -0.0992999  0.1055640  -0.941    0.347    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 32.36 on 4356 degrees of freedom
Multiple R-squared:  0.8819,    Adjusted R-squared:  0.8816 
F-statistic:  3251 on 10 and 4356 DF,  p-value: < 2.2e-16
Predicted AQI: 179.675 
RMSE: 29.14142 
                            [ggplot & lattice plot also]
'''

