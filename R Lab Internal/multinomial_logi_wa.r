library(ggplot2)
library(nnet)

data<-read.csv("multinomial_logi.csv")
data

model<-multinom(Purchase_Category~Age+Income,data=data)
model

summary(model)

a_age <- as.integer(readline("Enter the age of the individual: "))
a_income <- as.integer(readline("Enter the income of the individual: "))
a <- data.frame(Age = a_age, Income = a_income)

prob<-predict(model,a,type = "probs")
prob
prediction<-predict(model,a,type="class")
prediction[1]

pred<-c(unique(data$Purchase_Category))

cat(pred[prediction])

as.numeric(data$Purchase_Category)

plot(data$Age,as.numeric(data$Purchase_Category),col="red")
