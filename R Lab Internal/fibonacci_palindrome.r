df1<-data.frame(ID=c(1,2,3,4),name=c('a','b','c','d'))
df2<-data.frame(ID=c(1,2,3,4),sal=c(9,8,7,6))
dfm<-merge(df1,df2,by="ID")
dfm

find_min_max<-function(df){
  max_row<-apply(df,1,max)
  min_row<-apply(df,1,min)
  max_col<-apply(df,2,max)
  min_col<-apply(df,2,min)
  return (list(max_row=max_row,min_row=min_row,max_col=max_col,min_col=min_col))
  
}
dfmm <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
dfmm
find_min_max(dfmm)

max(dfmm)

fibonacci<-function(n){
  fibs<-numeric(n)
  fibs[1]<-1
  if (n>1) fibs[2]<-1
  for (i in 3:n){
    fibs[i]<-fibs[i-2]+fibs[i-1]
  }
  return (fibs)
}


check_fibonacci_in_df<-function(df){
  fibs<-fibonacci(max(df))
  fin<-c()
  for (i in df){
    for (j in i){
      if (j %in% fibs){
        fin<-c(fin,j)
        cat("Fibonacci number",j,"found in df\n")
        cat("factorial of ",j," is ",factorial(j),"\n")
      }
    }
  }
  fin
  lapply(fin,factorial)
}

check_fibonacci_in_df(dfmm)

