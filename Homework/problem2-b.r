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
createCSV<-function(primes){
  file.create("primes.csv")
  write.table(x = primes, 
           file = "primes.csv")

}
getPrimes<-function(number){
  primes = c()
  for (i in 2:number){
    if (isPrime(i)==TRUE){
      primes=c(primes,i)
    }
  }
  return(primes)
}

#stringInput = readline(prompt='Enter your number:')
number = 100000
primes = getPrimes(number)
createCSV(primes)

