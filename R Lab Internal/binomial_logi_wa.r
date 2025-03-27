library(ggplot2)

data<-read.csv("binomial_logi.csv")
data

model<-glm(Obese~Height+Age,data=data,family=binomial)

model

summary(model)

ah<-as.integer(readline(prompt = "Enter Height : "))
aa<-as.integer(readline(prompt = "Enter Age : "))

a<-data.frame(Height=ah,Age=aa)

prob<-predict(model,a,type="response")
prob
prediction<-ifelse(prob>0.5,1,0)

prediction

plot(data$Height,data$Obese,col="red",main="Bin logi",cex=1.3,pch=16,xlab="Height in cm",ylab="Obese")
