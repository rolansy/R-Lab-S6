data<-read.csv('data2.csv',row.names = 1)

data
variance_yes<-var((data$yes))
variance_no<-var((data$no))

covariance<-cov(data$yes,data$no)

chi_square_test<-chisq.test(data)
chi_square_test

chi_square_values<-as.vector(chi_square_test$observed - chi_square_test$expected)
chi_square_values
chi_square_sum<-sum(chi_square_values^2/chi_square_test$expected)
chi_square_sum

plot_data<-data.frame(
  Category=rownames(data),
  ChiSquareValues=chi_square_values
)

plot(data,chi_square_values)

boxplot(data$yes,data$no,names = c('yes','no'),main="Box plot of data",na.rm=TRUE)
