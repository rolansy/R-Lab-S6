data<-read.csv("multiple_reg.csv")

data

model<-lm(Weight~Height+Age,data=data)
model
summary(model)

ah<-as.integer(readline(prompt="Enter Height : "))
aa<-as.integer(readline(prompt="Enter Age : "))

