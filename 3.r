df1<-data.frame(a=c(1,2,3),b=c(4,5,6))
df2<-data.frame(a=c(7,8,9),b=c(10,11,12))
df1
df2
combine_datasets<-function(df1,df2,method='rbind'){
  if (method=='rbind'){
    return (rbind(df1,df2))
  }
}
print(combine_datasets(df1,df2))

dfp <- data.frame(A = c(1, 2, 3), B = c(2, 1, 2), C = c(3, 2, 1))

is_palindrome<-function(df){
  mat<-as.matrix(df)
  rownames(mat) <- NULL
  colnames(mat) <- NULL
  return (identical(mat,t(mat)))
}

print(is_palindrome(dfp))

find_min_max<-function(df){
  max_row<-apply(df,1,max)
  min_row<-apply(df,1,min)
  max_col<-apply(df,2,max)
  min_col<-apply(df,2,min)

}

fibonacci<-function(n){
  fibs<-numeric(n)
  fibs[1]<-1
  if (n>1) fibs[2]<-1
  for (i in 3:n){
    fibs[i]<-fibs[i-1]+fibs[i-2]
  }
  return (fibs)
}

print(fibonacci(10))

factorial_of_fibonacci<-function(n){
  fibs<-fibonacci(n)
  return (sapply(fibs,factorial))
}

print(factorial(6))

