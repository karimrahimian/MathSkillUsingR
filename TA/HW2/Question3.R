fibonachi<- function (n){
    if (n==0)
        return (0)
    else if (n==1 || n==2)
        return(1)
    else {
        return (fibonachi(n-1)+fibonachi(n-2))
    }
}
stringInput = readline(prompt='Enter your number:')
number = as.integer(stringInput)

result = fibonachi(number)
print (paste("The nth sequence of fibonachi is ",result))
