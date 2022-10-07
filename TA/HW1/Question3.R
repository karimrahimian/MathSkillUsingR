getMax <-function(dataList){
  listSize = length(dataList)
  maximum = dataList[1]
  for(i in 2:listSize){
    if (dataList[i]>maximum){
      maximum = dataList[i]
    }
  }
  return (maximum)
}
getMin <-function(dataList){
  listSize = length(dataList)
  minimum = dataList[1]
  for(i in 2:listSize){
    if (dataList[i]<minimum){
      minimum = dataList[i]
    }
  }
  return (minimum)
}
calcAverage <-function(dataList){
  listSize = length(dataList)
  sumation = 0
  for(i in 1:listSize){
      sumation = sumation + dataList[i]
  }
  average = sumation /listSize
  return (average)
}

dataList = c(1,2,30,4,5,2,-1)

maximum = getMax(dataList)
minimum = getMin(dataList)
mean = calcAverage(dataList)

print(paste("Maximum = " ,maximum))
print(paste("Minimum = " ,minimum))
print(paste("Average = " ,mean))