isPrime <- function(number){
  is_prime=TRUE
  half_number = number / 2
  if (number == 2)
    return (TRUE)
  roots = sqrt(number)
  for (i in 2:roots){
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
  