data<-read.csv("linear_reg.csv")
data

model<-lm(Weight ~ Height,data=data)
