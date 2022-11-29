drawHeatmap<-function (heat){
  xSize <- ncol(heat)
  ySize <- nrow(heat)
  maxColorSize = 200
  plot (c(-1,xSize+2),
        c(-1,ySize+2),
        type= 'n',
        axes = FALSE, 
        ann=FALSE)

  colfun = colorRampPalette(c("red","blue"))
  colours = colfun(maxColorSize)
  # loops for create heatmap using polygon statement
  for(i in ySize:1){
    for (j in 1:xSize){
      x= heat[i,j]
      xCoords = c(j,j+1,j+1,j)
      yCoords = c(i,i,i-1,i-1)
      polygon(xCoords,yCoords,col = colours[round(x*maxColorSize)-1])
    }
  }
}

rowSize = 10
colSize = 8
#create a random matrix
outMatrix = matrix(runif(rowSize*colSize),rowSize,colSize)
drawHeatmap(outMatrix)
