combine_datasets<-function(df1,df2,method='rbind'){
  if (method=='rbind'){
    return (rbind(df1,df2))
  }
}

is_palindrome<-function(df){
  print(df)
  mat<-as.matrix(df)
  rownames(mat) <- NULL
  colnames(mat) <- NULL
  return (identical(mat,t(mat)))
}

find_min_max<-function(df){
  max_row<-apply(df,1,max)
  min_row<-apply(df,1,min)
  max_col<-apply(df,2,max)
  min_col<-apply(df,2,min)
  return(list(max_row = max_row, min_row = min_row, max_col = max_col, min_col = min_col))
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

factorial_of_fibonacci<-function(n){
  fibs<-fibonacci(n)
  print(sapply(fibs,factorial))
}

check_fibonacci_in_dataframe<-function(df){
  fibs <- fibonacci(max(df))
  fin <- c()
  for (i in df) {
    for (j in i) {
      if (j %in% fibs) {
        fin <- c(fin, j)
        cat("Fibonacci number ", j, " found in dataframe.\n")
        cat("Factorial of ", j, " is ", factorial(j), "\n")
      }
    }
  }
  fin
  if (length(fin)==0){
    cat("No Fibonacci numbers found in dataframe.\n")
  }else{
    cat("Fibonacci numbers in dataframe :\n")
    print(fin)
    cat("Factorials of Fibonacci numbers in dataframe:\n")
    print(lapply(fin,factorial))
  }
  
}

menu<-function(){
  cat("Menu : \n")
  print("1. Combine two datasets")
  print("2. Check if dataframe is palindrome")
  print("3. Find max and min of each row and column of a dataframe")
  print("4. Check for Fibonacci numbers in dataframe and display their factorials")
  print("5. Exit")
  choice<-as.integer(readline(prompt = "Enter your Choice : "))
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
      df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
      check_fibonacci_in_dataframe(df)
    }else if (choice ==5){
      cat("Exiting...\n")
      break
    }else{
      cat("Invalid choice!")
    }
  }
}

main()
