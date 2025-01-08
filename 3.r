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


menu<-function(){
  cat("Menu : \n")
  print("1. Combine two datasets")
  print("2. Check if dataframe is palindrome")
  print("3. Find max and min of each row and column of a dataframe")
  print("4. Find factorial of nth value of Fibonacci series")
  print("5. Exit")
  choide<-as.integer(readline(prompt = "Enter your Choice : "))
  return (choice)
}

main<-function(){
  repeat{
    choice<-menu()
    if (choice==1){
      df1 <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
      df2 <- data.frame(a = c(7, 8, 9), b = c(10, 11, 12))
      cat("Df1 : \n")
      print(df1)
      cat("Df2 : \n")
      print(df2)
      combine_datasets(df1,df2)
    }else if (choice ==2){
      dfp<-data.frame(a=c(1,2,3),b=c(2,1,2),c=c(3,2,1))
      print(is_palindrome(dfp))
    }else if(choice ==3){
      df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
      print(find_min_max(df))
    }else if (choice==4){
      n<-as.integer(readline(prompt = "Enter which term of FIBONACCI SERIES : "))
      print(factorial_of_fibonacci(n))
    }else if (choice ==5){
      cat("Exiting...\n")
      break
    }else{
      cat("Invalid choice!")
    }
  }
}

main()
