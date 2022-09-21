sqrt<-function (x,tolerance){
  counter = 0
  while (TRUE==TRUE){
    roots = counter* counter
    if (roots < x )
    {
      counter=counter+1
      next
    }
    else
    {
      if (roots == x)
        break
      if (roots > x)
      {
        counter = counter-1
        while(TRUE==TRUE)
        {
          roots = counter*counter
          if (roots < x )
          {
            counter=counter+ tolerance
            next
          }
          else
          {
            if (counter>x)
              counter = counter - tolerance
            break
          }
        }
        break
      }
    }
  }
  return(counter)
}
tolerance = 0.0001
number = 10
result = sqrt(number,tolerance)
print(result)