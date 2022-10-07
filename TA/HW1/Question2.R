addVectors<-function(firstVector,secondVector){
    result=integer(length(firstVector))
    listSize = length(firstVector)
    for(i in 1: listSize){
        result[i] = firstVector[i] + secondVector[i]
    }
    return (result)
}
vector1 = c(1,2,3,4,5,6)
vector2 = c(2,1,3,3,5,6)

result = addVectors(vector1,vector2)
print(paste(result))
