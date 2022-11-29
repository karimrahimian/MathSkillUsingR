calcFactorial<-function(k){
  start = 1000000-k+1
  fact = 1
  for (i in start:1000000){
    fact = fact*i
  }
  return(fact)
}
power<-function(k){
  result<- 1
  for(i in 1:k){
    result = result * 1000000
  }  
  return(result)
}
probability<-function(k){
  result = 1- (calcFactorial(k)/power(k))
  return(result)
}
getProbabilites<-function(maxK){
  result <- c()
  for (k in 1:maxK){
    prob= probability(k)
    result<-c(result,prob)
  }
  return(result)
}
plotChart<-function(maxK,probs){
  plot.new()
  plot.window(xlim=c(0,maxK), ylim=c(0,1))
  axis(1)
  axis(2)
  x = 1:maxK
  y =  probs

  plot(x,y, lwd=2, col="blue")
  
  lines(c(0,100),c(0.5,0.5), lwd=2, col="red",lty='dotted')
  
}

maxK = 1000
probs <- getProbabilites(maxK)
plotChart(maxK, probs)


