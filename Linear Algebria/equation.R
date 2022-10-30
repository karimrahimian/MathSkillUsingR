
setwd("~/Desktop/university")
data = read.csv('equation.csv',sep=",")

eliminateVaraible <-function (data){
  rowSize = nrow(data)
  X = data
  rowSize = nrow(X)
  for (k in 1:(rowSize-1)){
    for(i in (k+1):rowSize){
      for (j in k:rowSize){
        rcoef = X[i,k] / X[k,k] 
        X[i,j] = X[i,j] - rcoef * X[k,j]
      }
    }
  }
  return (X)
}
findSolution<-function(X){
  colSize = ncol(X)
  rowSize = nrow(X)
  solutions = numeric(rowSize)
  B = X[,colSize]
  
  solutions[rowSize] = X[rowSize,(colSize-1)] / -X [rowSize,(colSize)]
  for (i in (rowSize-1):1){
    r_coef = 0
    for (j in (i+1):colSize){
      if (j!=colSize)
        r_coef = solutions[j]*X[i,j] + r_coef
      else
        r_coef = X[i,j] + r_coef
    }
    solutions[i] = r_coef / -X[i,colSize]
  }
  return (solutions)
  
}

X = eliminateVaraible(data)
sol = findSolution(X)
print(sol)