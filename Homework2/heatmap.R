library(dichromat)

heatmap<-function (heat){
  xLength <- ncol(heat)
  yLength <- nrow(heat)
  plot (c(0,xLength+3),c(0,yLength+3),type= 'n',axes = FALSE, ann=FALSE)
  axis(1)
  axis(2)
  colfun = colorRampPalette(c("red","orange"))
  colours = colfun(101)
  for(i in yLength:1){
    for (j in 1:xLength){
      x= heat[i,j]
      xCoords = c(j,j+1,j+1,j)
      yCoords = c(i,i,i-1,i-1)
      polygon(xCoords,yCoords,col = colours[round(x*100)-1])
    }
  }
}
polyGun<-function (){
  plot (c(0,3),c(0,3),type= 'n',axes = FALSE, ann=FALSE)
  axis(1)
  axis(2)
  xCoords = c(0,0,1,1)
  yCoords = c(0,1,1,0)
  polygon(xCoords,yCoords,col = 'red')
}

#data = matrix(runif(4*3),4,3)
#heatmap((data))
polyGun()
