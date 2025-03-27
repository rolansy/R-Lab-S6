data<-read.csv("linear_reg.csv")
data

model<-lm(Weight ~ Height,data=data)
summary(model)
model_sum<-summary(model)
model_sum_coef<-model_sum$coefficients

a<-as.integer(readline(prompt = "Enter height : "))
a<-data.frame(Height=a)
a
result<-predict(model,a)
result

plot(data$Height,data$Weight,col="red",main="Linear Regression",cex=1.3,pch='*',)
abline(model,col="blue",lty=1)

