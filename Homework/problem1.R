createProductTable<-function(nrow,ncol){
  productTable = matrix(0,10,10)
  for (row in 1:10){
      for(col in 1:10){
          productTable[row,col]=row*col
      }
  }
  return(productTable)
}
saveTableToPDF<-function(filename,table){
  pdf(filename, paper="a4")
  plot.new()
  plot.window(xlim=c(0,11), ylim=c(0,11))

  rows = nrow(table);
  cols = ncol(table);
  text(x=5,y=11,'Product Table design by karim ')
  for (row in 1:rows){ 
    for(col in 1:cols){
      x = row
      y = (cols-col)+1
      lines(c(x-0.5,x+0.5),c(y-0.5,y-0.5), lwd=2, col="blue")
      lines(c(x-0.5,x+0.5),c(y+0.5,y+0.5), lwd=2, col="blue")
      lines(c(x-0.5,x-0.5),c(y-0.5,y+0.5), lwd=2, col="blue")
      lines(c(x+0.5,x+0.5),c(y+0.5,y-0.5), lwd=2, col="blue")
      text(
            x=x, 
            y=y, 
            as.character(table[row,col])
          )  
    }
  }
  dev.off()
}
n = 10
productTable = createProductTable(n,n)
saveTableToPDF("table.pdf",productTable)

