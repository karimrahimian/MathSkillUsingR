nums = list(1,2,3,4,5,2)
target = 5
for (i in 1:length(nums))
  for (j in 1:length(nums))
    if (i<=j)
    {
      two_sum = nums[[i]]+nums[[j]]
      if (two_sum==target)
      {   
        #result = cat(i,j,sep = ',')
        print(paste(i,j))
      }
    }
