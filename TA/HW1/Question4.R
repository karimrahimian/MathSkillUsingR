isOdd <- function(number){
  remainder = number %% 2
  if (remainder == 1){
    return(TRUE)
  }else {
    return(FALSE)
  }
}
stringInput = readline(prompt='Enter your number:')
number = as.integer(stringInput)
result = isOdd(number)
if (result == TRUE){
  print('your number is Odd')
}else {
   print('Your number is Even')
}
