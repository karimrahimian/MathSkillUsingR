isOdd <- function(number){
  remainder = number %% 2
  if (remainder == 1)
    return(TRUE)
  else
    return(FALSE)
}
bubbleSort<-function(numbers){
  phase = 0
  while(1) {
    swamps_count = 0
    for (j in 1 : (length(numbers) - 1 - phase)) {
      if (numbers[j] > numbers[j + 1]) {
        s = numbers[j]
        numbers[j] = numbers[j+1]
        numbers[j+1] = s
        swamps_count = swamps_count + 1
      }
    }
    phase = phase + 1
    if(swamps_count == 0) 
      break
  }
  return (numbers)
}
getMaximum <-function(numbers){
  max<- numbers[1]
  for(i in 2:length(numbers)){
    if (numbers[i]> max )
      max<- numbers[i]
  }
  return (max) 
}
getMinimum <-function(numbers){
  min<- numbers[1]
  for(i in 1:length(numbers)){
    if (numbers[i]< min )
      min<- numbers[i]
  }
  return (min) 
}
getMean <-function(numbers){
  sum <- 0 
  for(i in 1:length(numbers)){
      sum =sum+numbers[i]
  }
  return (sum/length(numbers)) 
}
getMedianIndex <-function(array){
  return (ceiling(length(array)/2))
}
isOddArrayLength<-function(array){
  return (isOdd(length(array)))
}
getMedianValue<-function(sortedArray){
  if (isOddArrayLength(sortedArray)==TRUE){
    medianIndex = getMedianIndex(sortedArray)
    medianValue = sortedArray[medianIndex]
  } 
  else{
    medianIndex = getMedianIndex(sortedArray)
    medianValue = (sortedArray[medianIndex]+sortedArray[medianIndex+1])/2
  }
  return(medianValue)
}

getQuantile1<-function(sortedArray,medianIndex){
  if (isOdd(length(sortedArray))==TRUE){
    leftArray = sortedArray[1:medianIndex-1]
    medianValue = getMedianValue(leftArray)
  }
  else{
    leftArray = sortedArray[1:medianIndex]
    medianValue = getMedianValue(leftArray)
  }
  quantile1 = medianValue
  return (quantile1)
}
getQuantile2<-function(sortedArray,medianIndex){
  arrayLength = length(sortedArray)
  if (isOdd(length(sortedArray))==TRUE){
    medianIndex = medianIndex+1
    rightArray = sortedArray[medianIndex:arrayLength]
    medianValue = getMedianValue(rightArray)
  }
  else{
    medianIndex = medianIndex+1
    rightArray = sortedArray[medianIndex:length(sortedArray)]
    medianValue = getMedianValue(rightArray)
  }
  quantile2= medianValue
  return (quantile2)
}
drawBox<-function(x,min,max,mean,q1,q2){
  boxWidth = 0.3
  plot.new()
  plot.window(xlim=c(length(x)-1,length(x)+1), ylim=c(min-1,max+1))
  axis(1)
  axis(2)
  minX = c(x-boxWidth,x+boxWidth)
  minY = c(min,min)
  lines(minX,minY, lwd=2, col="blue")
  
  maxX = c(x-boxWidth,x+boxWidth)
  maxY = c(max,max)
  lines(maxX,maxY, lwd=2, col="blue")

  meanX = c(x-boxWidth,x+boxWidth)
  meanY = c(mean,mean)
  lines(meanX,meanY, lwd=2, col="red")
  
  q1X = c(x-boxWidth,x+boxWidth)
  q1Y = c(q1,q1)
  lines(q1X,q1Y, lwd=2, col="blue")
  
  q2X = c(x-boxWidth,x+boxWidth)
  q2Y = c(q2,q2)
  lines(q2X,q2Y, lwd=2, col="blue")
  
  buttomLineX = c(x,x)
  buttomLineY = c(min,q1)
  lines(buttomLineX,buttomLineY, lwd=2, col="blue")
 
  upperLineX = c(x,x)
  upperLineY = c(q2,max)
  lines(upperLineX,upperLineY, lwd=2, col="blue")   
  
  leftLineX = c(x-boxWidth,x-boxWidth)
  leftLineY = c(q1,q2)
  lines(leftLineX,leftLineY, lwd=2, col="blue")   
  
  
  rightLineX = c(x+boxWidth,x+boxWidth)
  rightLineY = c(q1,q2)
  lines(rightLineX,rightLineY, lwd=2, col="blue")   
  
  
  title(xlab = "Group")
  title(ylab = "Y-axis value")
  
  box()
}


data = sample(1:100,30)
sorted_data = bubbleSort(data)
print(sorted_data)
medianIndex = getMedianIndex(sorted_data)
mean_data = getMean(sorted_data)
q1 = getQuantile1(sorted_data,medianIndex)
q2 = getQuantile2(sorted_data,medianIndex)
mean = getMean(sorted_data)
min = sorted_data[1]
max = sorted_data[length(sorted_data)]
drawBox(1,min,max,mean,q1,q2)
