bubbleSort<-function(numbers){
  phase = 0
  while(1) {
    swamps_count = 0
    for (j in 1 : (length(numbers) - 1 - phase)) {
      if (numbers[j] > numbers[j + 1]) {
        numbers[j] = numbers[j]+numbers[j+1]
        numbers[j+1] = numbers[j] - numbers[j+1]
        numbers[j] = numbers[j] - numbers[j+1]
        swamps_count = swamps_count + 1
      }
    }
    phase = phase + 1
    if(swamps_count == 0) 
      break
  }
  return (numbers)
}
myList = c(2,3,4,5,6,1,2,3,4,4,5)
sortedList = bubbleSort(myList)
print (sortedList)