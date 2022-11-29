nums = list(1,3,4,5)
target = 6
founded = FALSE
for (i in 1:length(nums))
  for (j in 1:length(nums))
    if (i<j)
    {
      two_sum = nums[[i]]+nums[[j]]
      if (two_sum==target){   
        print(paste(i,j))
        founded = TRUE
      }
    }

if (founded==FALSE){
  print("Does not found any things")
}

