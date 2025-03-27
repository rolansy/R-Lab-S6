library(C50)
library(e1071)
library(ggplot2)
library(caTools)

data<-read.csv("svm.csv")

data
data=data[,3:5]
data
data$Purchased
data$Purchased=factor(data$Purchased,levels = c(0,1))
data$Purchased
set.seed(123)
split=sample.split(data$Purchased, SplitRatio=0.75)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)
train
test
length(test[,1])
length(train[,1])
length(data[,1])

train[,1:2]=scale(train[,1:2])
train
test[,1:2]=scale(test[,1:2])
test
classifier<-svm(formula=Purchased~.,
                data=train,
                type="C-classification",
                kernel="radial"
                )

y_pred=predict(classifier,newdata = test[,-3])
cm=table(test[,3],y_pred)
cm

plot_decision_boundary<-function(set,title){
  x1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
  x2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
  
  grid_set=expand.grid(x1,x2)
  colnames(grid_set)=c('Age','EstimatedSalary')
  y_grid=predict(classifier,newdata = grid_set)
  
  plot(
    set[,-3],
    main=title,
    col=ifelse(set[,3]==1,'green','red')
  )
  
  contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),
          add=TRUE,drawlabels = TRUE,lwd=2,col="blue"
          )
  points(grid_set,pch='.',col=ifelse(y_grid==1,'lightgreen','lightpink'))
  points(set,pch=21,bg=ifelse(set[,3]==1,'green','red'),cex=1)
  
}

plot_decision_boundary(train,'train')

plot_decision_boundary(test,'test')

pred1<-function(){
  new_age = as.numeric(readline(prompt = "Enter Age : "))
  new_salary = as.numeric(readline(prompt = "Enter Salary : "))
  
  if (is.na(new_age) | is.na(new_salary)){
    stop("INvalid")
  }
  
  new_data<-data.frame(Age=(new_age-mean(data$Age))/sd(data$Age),
                       EstimatedSalary=(new_salary-mean(data$EstimatedSalary))/sd(data$EstimatedSalary))
  
  prediction=predict(classifier,newdata = new_data)
  print(prediction)
  
  if (prediction == 1) {
    cat("\nPrediction Result:\n The person is likely to Purchase.\n")
  } else {
    cat("\nPrediction Result:\n The person is NOT likely to Purchase.\n")
  }

    
}  

pred1()


tendata<-read.csv('tennis.csv')
tendata$Outlook<-factor(tendata$Outlook)
tendata$Temperature<-factor(tendata$Temperature)
tendata$Humidity<-factor(tendata$Humidity)
tendata$Wind<-factor(tendata$Wind)
tendata$PlayTennis<-factor(tendata$PlayTennis)
tendata$Outlook

model<-C5.0(PlayTennis~Outlook+Wind+Humidity+Temperature,data=tendata)

model
plot(model)
plot(model,type="simple",main="Decision tree")
library(partykit)

tree<-as.party(model)
plot(tree,type="simple")
summary(model)

get_inp<-function(){
  outlook <- readline(prompt = "Enter Outlook (Sunny, Overcast, Rain): ")
  temperature <- readline(prompt = "Enter Temperature (Hot, Mild, Cool): ")
  humidity <- readline(prompt = "Enter Humidity (High, Normal): ")
  wind <- readline(prompt = "Enter Wind (Weak, Strong): ")
  
  return (data.frame(Outlook=factor(outlook),Temperature=factor(temperature),Humidity=factor(humidity),Wind=factor(wind)))
  
}

test<-get_inp()
prediction<-predict(model,test)
prediction
cat(prediction)
prediction=="Yes"
