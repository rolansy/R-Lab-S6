data<-read.csv("multiple_reg.csv")

data

model<-lm(Weight~Height+Age,data=data)
model
summary(model)

ah<-as.integer(readline(prompt="Enter Height : "))

aa<-as.integer(readline(prompt="Enter Age :"))

a<-data.frame(Height=ah,Age=aa)

result<-predict(model,a)
result

plot(data)


