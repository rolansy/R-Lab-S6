a<-c(10,15,20,25,30,35,40,45,50)
b<-c(5,10,15,20,25,30,35,40,45)
cat('\nData A : ',a)
cat('\nMean : ',mean(a))
cat('\nMedian : ',median(a))
cat('\nSummary : ',summary(a))
s1<-'con'
s2<-'cat'
cat('\nMin : ',min(a))
cat('\nMax : ',max(a))
cat('\n1st Qaurtile : ',quantile(a,0.25))
cat('\n3st Qaurtile : ',quantile(a,0.75))
cat('\nSummary : ',summary(a))

cat('\nVariance : ',var(a))
boxplot(a,main='Boxplot of A',col='cyan')

cat('\nData B : ',b)
cat('\nCovariance of A and B : ',cov(a,b))
cat('\nCorrelation of A and B : ',cor(a,b))




