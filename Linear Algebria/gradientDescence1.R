gradiantD <- function(X,Y){
  solumn_size = ncol(X)
  alpha = 0.001
  B = runif(n=solumn_size, 
            min = 0, 
            max = 1)
  m = nrow(X)  

  for (i in 1:100){
    for (k in 1:length(B)){
      r = 0
      for (i in 1:m){
        yhat =  abs((sum(B*X[i,]))-Y[i]) 
        r = r + X[i,k] * yhat
      }
      B[k] = B[k] - (alpha * r)
    }
  }
  return (B)
}

m = 100
d = 4

X = matrix(runif(n=sampleSize, min = 0, max = 1)*0.05,
                 nrow=m, 
                 ncol=d)
Y = matrix(sample(rows),nrow = m,ncol=1)
B = gradiantD(X,Y)
print (B)
