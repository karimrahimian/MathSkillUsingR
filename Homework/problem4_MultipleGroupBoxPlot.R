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
drawMultipleGroupBox<-function(x,minimum,maximum,mean,q1,q2){
  boxWidth = 0.3
  plot.new()
  plot.window(xlim=c(0,nrow(x)*ncol(x)+1), ylim=c(min(minimum)-1,max(maximum)+1))
  axis(1)
  axis(2)
  colors = c('blue','black','red','cyan')
  mean_colors = c('red','green','blue','black')
  for(i in 1:nrow(x)){
    for (j in 1:ncol(x))
    {
      minX = c(x[i,j]-boxWidth,x[i,j]+boxWidth)
      minY = c(minimum[i,j],minimum[i,j])
      lines(minX,minY, lwd=2, col=colors[j])
      
      maxX = c(x[i,j]-boxWidth,x[i,j]+boxWidth)
      maxY = c(maximum[i,j],maximum[i,j])
      lines(maxX,maxY, lwd=2, col=colors[j])
      
      meanX = c(x[i,j]-boxWidth,x[i,j]+boxWidth)
      meanY = c(mean[i,j],mean[i,j])
      lines(meanX,meanY, lwd=2, col=mean_colors[j])
      
      q1X = c(x[i,j]-boxWidth,x[i,j]+boxWidth)
      q1Y = c(q1[i,j],q1[i,j])
      lines(q1X,q1Y, lwd=2, col=colors[j])
      
      q2X = c(x[i,j]-boxWidth,x[i,j]+boxWidth)
      q2Y = c(q2[i,j],q2[i,j])
      lines(q2X,q2Y, lwd=2, col=colors[j])
      
      buttomLineX = c(x[i,j],x[i,j])
      buttomLineY = c(minimum[i,j],q1[i,j])
      lines(buttomLineX,buttomLineY, lwd=2, col=colors[j])
      
      upperLineX = c(x[i,j],x[i,j])
      upperLineY = c(q2[i,j],maximum[i,j])
      lines(upperLineX,upperLineY, lwd=2, col=colors[j])   
      
      leftLineX = c(x[i,j]-boxWidth,x[i,j]-boxWidth)
      leftLineY = c(q1[i,j],q2[i,j])
      lines(leftLineX,leftLineY, lwd=2, col=colors[j])   
      
      rightLineX = c(x[i,j]+boxWidth,x[i,j]+boxWidth)
      rightLineY = c(q1[i,j],q2[i,j])
      lines(rightLineX,rightLineY, lwd=2, col=colors[j])   
    }
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

#sortedIndex = order(mean)
x = matrix( c(1,2,3,4,5,6),nrow=3, ncol=2,byrow = TRUE)
min1 = matrix( c(1,1.5,0.5,1,2,1.5),nrow=3, ncol=2,byrow = TRUE)
max1 = matrix( c(10,10.5,10.5,13,12,10.5),nrow=3, ncol=2,byrow = TRUE)
mean = matrix( c(5,5.5,6.5,7,3.5,4.5),nrow=3, ncol=2,byrow = TRUE)

q1 = matrix( c(2,2.5,1.5,2,3,2.5),nrow=3, ncol=2,byrow = TRUE)
q2 = matrix( c(9,9.5,9.5,9,11,9.5),nrow=3, ncol=2,byrow = TRUE)

drawMultipleGroupBox(x,min1,max1,mean,q1,q2)

