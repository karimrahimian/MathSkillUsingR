JB <-function (X,Y,B,k){
  sampleSize = nrow(X)
  result = 0
  for (i in 1:sampleSize){
    newy =  abs((sum(B*X[i,]))-Y[i]) 
    result = result+ X[i,k] * newy
  }
  return (result)
}

gradientDescesnce <- function(X,Y,lr){
  featureCount = ncol(X)
  #initial coefficient with small random values
  B = runif(n=featureCount, min = 0, max = 1)
  for (i in 1:5){
    for (k in 1:length(B)){
      answer = JB(X,Y,B,k)
      B[k] = B[k] - (lr * answer)
    }
  }
  return (B)
}

sampleSize = 100
rows = 20
cols = 5

X = matrix(runif(n=sampleSize, min = 0, max = 1)*0.05, nrow=rows, ncol=cols)
Y = sample(rows)
learningRate = 0.01
B = gradientDescesnce(X,Y,learningRate)
print (B)

