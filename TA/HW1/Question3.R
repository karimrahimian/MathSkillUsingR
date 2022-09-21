findMaximum <-function(firstNum,secondNum){
  if (firstNum<secondNum)
    result <- firstNum
  else
    result <- secondNum
  return(result)
}

max= findMaximum(5,2)
print(max)