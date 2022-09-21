isOdd <- function(number){
  remainder = number %% 2
  if (remainder == 0)
    return('Your number is even')
  else
    return('Your number is odd')
}
stringInput = readline(prompt='Enter your number:')
number = as.integer(stringInput)
result = isOdd(number)
print(result)