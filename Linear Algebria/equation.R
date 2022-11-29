
data = read.csv('equation.csv',sep=",")
eliminateVaraible <-function (data){
  rowSize = nrow(data)
  colSize = ncol(data)
  X = data
  rowSize = nrow(X)
  for (k in 1:(rowSize-1)){
    for(i in (k+1):rowSize){
      rcoef = X[i,k] / X[k,k]
      for (j in k:colSize){
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
  solutions[rowSize] = X [rowSize,(colSize)] /X[rowSize,(colSize-1)]
  for (i in (rowSize-1):1){
    r_coef = 0
    for (j in (i+1):(colSize-1)){
        r_coef = solutions[j]*X[i,j] + r_coef
    }
    solutions[i] = (X[i,colSize] - r_coef) / X[i,i]
  }
  return (solutions)
}

X = eliminateVaraible(data)
sol = findSolution(X)
print(sol)
