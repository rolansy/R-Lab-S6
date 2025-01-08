a<-c(10,15,20,25,30,35,40,45,50)
b<-c(5,10,15,20,25,30,35,40,45)
cat('Data A : ',a)
cat('Mean : ',mean(a))
cat('Median : ',median(a))
cat('Summary : ',summary(a))
s1<-'con'
s2<-'cat'
cat('Min : ',min(a))
cat('Max : ',max(a))
cat('1st Qaurtile : ',quantile(a,0.25))
cat('3st Qaurtile : ',quantile(a,0.75))
cat('Summary : ',summary(a))
cat('Concat of ',s1,'and',s2,end="")
cat(s1,s2,sep='')
cat('Variance : ',var(a))
boxplot(a,main='Boxplot of A',col='cyan')

cat('Data B : ',b)
cat('Covariance of A and B : ',cov(a,b))
cat('Correlation of A and B : ',cor(a,b))




