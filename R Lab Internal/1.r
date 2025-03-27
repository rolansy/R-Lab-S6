a<-c(1,2,3,4,6,10,22,25,100)
b<-c(5,10,15,20,25,63,2,-100,75)
c<-b*2
c
cat("\n Data : ",a)
cat("\nMean : ",mean(a))
cat("\n Median : ",median(a))
cat("\n Summary : \n",summary(a))
summary(a)
s1<-'con'
s2<-'cat'
min(a)
max(a)
quantile(a,0.25)
var(a)
sd(a)
boxplot(a,main="Box plot of a",col="cyan")
cov(a,b)
cor(a,b)
cov(b,c)
cor(b,c)
cor(a,c)
cor(c,a)
