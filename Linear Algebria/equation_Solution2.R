


print("Enter your numb")
n = as.numeric(readline())
X = matrix(0, n,n)
Y = numeric(n)

for (i in 1:n){
  for (j in 1:n){
    print (paste("enter matrix (",i,j,")"))
    X[i,j] = as.numeric(readline())
  }
}
#Enter B values
for (i in 1:n){
  print(paste("Enter ",i,"th:"))
  Y[i] = as.numeric(readline())
}

X=cbind(X,Y)

n = nrow(X)
k = n - 1 
while (k >=1 ){
  for(i in (k+1):n){
    for (j in k:ncol(X)){
      X[i,j] = X[i,j] - (X[i,k] / X[k,k]) * X[k,j]
    }
  }
  k=k-1
}

m = ncol(X)
n = nrow(X)
ans = numeric(n)

ans[n] =X [n,m] / X[n,(m-1)]
i=n-1
while (i>=1){
  s = 0
  for (j in (i+1):(m-1)){
      s = s+ ans[j]*X[i,j]
  }
  ans[i] = (X[i,m]-s) / X[i,m]
  i=i-1
}

print (ans)
