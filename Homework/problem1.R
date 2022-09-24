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
  pdf("filename.pdf", paper="a4")
  plot.new()
  txt = 'Salam'
  for (row in 1:nrow(table)){
    for(col in 1:ncol(table)){
      text(x=row/10, y=col/10, as.character(table[row,col]))  
    }
  }
  dev.off()
  print ("your pdf saved in the ")
}
productTable = createProductTable(10,10)
saveTableToPDF("table.pdf",productTable)

