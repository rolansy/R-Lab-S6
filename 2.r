
data <- read.csv("/home/ds-da-14/Ronal/R-Lab-S6/data2.csv", row.names = 1)

variance_yes <- var(data$yes)
variance_no <- var(data$no)

covariance <- cov(data$yes, data$no)

coefficients <- cor(data$yes, data$no)

chi_square_test <- chisq.test(data)

print(paste("Variance (yes):", variance_yes))
print(paste("Variance (no):", variance_no))
print(paste("Covariance:", covariance))
print(paste("Coefficients (correlation):", coefficients))
print(chi_square_test)

chi_square_values <- as.vector(chi_square_test$observed - chi_square_test$expected)
chi_square_sum <- sum(chi_square_values^2 / chi_square_test$expected)

plot_data <- data.frame(
  Category = rownames(data),
  ChiSquareValues = chi_square_values
)

plot(data,chi_square_values)

boxplot(data$yes,data$no,names=c("Yes","No"),main="Boxplot of Yes and No",na.rm=TRUE)
