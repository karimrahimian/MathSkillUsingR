isPrime <- function(number){
  is_prime=TRUE
  half_number = number / 2
  if (number == 2)
    return (TRUE)
  for (i in 2:half_number){
    remainder = number %% i
    if (remainder == 0){
      is_prime = FALSE
    }
  }
  return (is_prime)
}

stringInput = readline(prompt='Enter your number:')
number = as.integer(stringInput)
result = isPrime(number)
print(result)
  