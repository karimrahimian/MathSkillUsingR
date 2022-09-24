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
  plot.window(xlim=c(length(x)-2,length(x)+2), ylim=c(min-1,max+1))
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
drawMultipleBox<-function(x,min,max,mean,q1,q2){
  boxWidth = 0.3
  plot.new()
  plot.window(xlim=c(0,length(x)+1), ylim=c(getMinimum(min)-1,getMaximum(max)+1))
  axis(1)
  axis(2)
  for(i in 1:length(x)){
    minX = c(x[i]-boxWidth,x[i]+boxWidth)
    minY = c(min[i],min[i])
    lines(minX,minY, lwd=2, col="blue")
    
    maxX = c(x[i]-boxWidth,x[i]+boxWidth)
    maxY = c(max[i],max[i])
    lines(maxX,maxY, lwd=2, col="blue")
    
    meanX = c(x[i]-boxWidth,x[i]+boxWidth)
    meanY = c(mean[i],mean[i])
    lines(meanX,meanY, lwd=2, col="red")
    
    q1X = c(x[i]-boxWidth,x[i]+boxWidth)
    q1Y = c(q1[i],q1[i])
    lines(q1X,q1Y, lwd=2, col="blue")
    
    q2X = c(x[i]-boxWidth,x[i]+boxWidth)
    q2Y = c(q2[i],q2[i])
    lines(q2X,q2Y, lwd=2, col="blue")
    
    buttomLineX = c(x[i],x[i])
    buttomLineY = c(min[i],q1[i])
    lines(buttomLineX,buttomLineY, lwd=2, col="blue")
    
    upperLineX = c(x[i],x[i])
    upperLineY = c(q2[i],max[i])
    lines(upperLineX,upperLineY, lwd=2, col="blue")   
    
    leftLineX = c(x[i]-boxWidth,x[i]-boxWidth)
    leftLineY = c(q1[i],q2[i])
    lines(leftLineX,leftLineY, lwd=2, col="blue")   
   
    rightLineX = c(x[i]+boxWidth,x[i]+boxWidth)
    rightLineY = c(q1[i],q2[i])
    lines(rightLineX,rightLineY, lwd=2, col="blue")   
  }

  title(xlab = "Group")
  title(ylab = "Y-axis value")
  
  box()
}
data <-matrix(rnorm(72),nrow=8,ncol=9)

mean=c()
min=c()
max=c()
q1=c()
q2=c()
x= c()
for(i in 1:ncol(data)){
  data[,i] = sort(data[,i])
  sorted_data = c(data[,i])
  medianIndex = getMedianIndex(data[,i])
  q1 =c(q1, getQuantile1(sorted_data,medianIndex))
  
  q2 =c(q2,getQuantile2(sorted_data,medianIndex))
  mean =c(mean,getMean(sorted_data))
  min = c(min, sorted_data[1])
  max = c(max,sorted_data[length(sorted_data)])
  x = c(x,i)
}
sortedIndex = order(mean)
drawMultipleBox(x,min[sortedIndex],max[sortedIndex],mean[sortedIndex],q1[sortedIndex],q2[sortedIndex])
  
