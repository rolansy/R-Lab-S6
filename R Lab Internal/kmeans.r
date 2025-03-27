library(ggplot2)
data<-read.csv("clustering_data.csv")

euclid_d<-function(a,b){
  return (sqrt(sum((a-b)^2)))
}

kmeansf<-function(data,k,max_iter=100){
  set.seed(123)
  centroids<-data[sample(1:nrow(data),k),]
  clusters<-numeric(nrow(data))
  old_centroids<-centroids
  iter<-0
  
  while (TRUE){
    iter<-iter+1
    for (i in 1:nrow(data)){
      distances<-apply(centroids,1,euclid_d,b=data[i,])
      clusters[i]<-which.min(distances)
    }
    
    for (j in 1:k){
      centroids[j,]<- colMeans(data[clusters==j,])
    }
    
    cat("Iteration ",iter," : \n")
    print(centroids)
    if (all(old_centroids==centroids) || iter>=max_iter ){
      break
    }
    
    old_centroids<-centroids
  }
  return (list(clusters=clusters,centroids=centroids))

}

kmeans_result<-kmeansf(data,2)

data$cluster<- as.factor(kmeans_result$clusters)

centroids_df<-as.data.frame(kmeans_result$centroids)
centroids_df

centroids_df$cluster<-as.factor(1:nrow(centroids_df))
centroids_df

data

ggplot(data,aes(x=x,y=y,color=cluster))+
  geom_point(size=3)+
  geom_point(data=centroids_df,aes(x=x,y=y),color="black",shape='*',size=10)+
  labs(title="Kmeans clustering",x='x axis',y='y axis')+
  theme_minimal()

data$cluster

