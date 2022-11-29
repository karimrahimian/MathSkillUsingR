library(maftools)

getOrderedUniqueHugoNames<-function(hugoNames){
  uniqueHugoNames = data.frame(table(hugoNames))
  filteredHugoNamesIndex= which(uniqueHugoNames$Freq>=5)
  filteredUniqueHugoNames = uniqueHugoNames[filteredHugoNamesIndex,]
  orderedUniqueHugoName = filteredUniqueHugoNames[order(filteredUniqueHugoNames$Freq,decreasing = TRUE),]
  return (orderedUniqueHugoName)
}

getOrderedUniqueSamples<-function(samples){
  uniqueSamples = data.frame(table(samples))
  filteredUniqueSamplesIndex= which(uniqueSamples$Freq>=1)
  filteredSample = uniqueSamples[filteredUniqueSamplesIndex,]
  orderedUniqueSample = filteredSample[order(filteredSample$Freq,decreasing = FALSE),]
  return(orderedUniqueSample)
}

getUniqueVariants<-function(variants){
  uniqueVariants = data.frame(table(variants))
  filteredUniqueVariantsIndex= which(uniqueVariants$Freq>=1)
  filteredVariants = uniqueVariants[filteredUniqueVariantsIndex,]
  orderedUniqueSample = filteredVariants[order(filteredVariants$Freq,decreasing = TRUE),]
  return(orderedUniqueSample)
}

calcSum <-function(vector){
  sum = 0
  vectorSize = length(vector)
  for (i in 1:length(vector)){
    sum = sum+vector[i]
  }
  return ((sum))
}

drawRightBoxPlot<-function(row,col,vector,maxdata){
  sum = calcSum(vector)
  nextStep = col

  colors = c('blue','purple','orange','green','red','black','yellow')
  for (k in 1:length(vector)){
    perc = ((vector[k] / maxdata))*20
    
    xCoords = c(nextStep, nextStep+perc, nextStep+perc, nextStep)
    nextStep = nextStep+perc
    yCoords = c(row-0.1,row-0.1,row-1+0.1,row-1+0.1)
    polygon(xCoords,yCoords,
            col = colors[k],
            border = NA)
  }
}

drawTopBoxPlot<-function(row,col,vector,maxdata){
  sum = calcSum(vector)
  nextStep = row
  colors = c('blue','purple','orange','green','red','black','yellow')
  for (k in 1:length(vector)){
    perc = ((vector[k] / maxdata))*3

    yCoords = c(nextStep, nextStep+perc, nextStep+perc, nextStep)
    nextStep = nextStep+perc
    xCoords = c(col-0.1,col-0.1,col-1+0.1,col-1+0.1)
    polygon(xCoords,yCoords,
            col = colors[k],
            border = NA)
  }
  
}

drawButtonBoxPlot<-function(row,col,vector){
  sum = calcSum(vector)
  nextStep = row
  #colors = c('blue','purple','orange','green','red','black','yellow')
  colors = c('pink','orange1','cyan')
  for (k in 1:length(vector)){
    perc = ((vector[k] / sum))*4
    yCoords = c(nextStep, nextStep+perc, nextStep+perc, nextStep)
    nextStep = nextStep+perc
    xCoords = c(col-0.1,col-0.1,col-1+0.1,col-1+0.1)
    polygon(xCoords,yCoords,
            col = colors[k],
            border = NA)
  }

  
}

putLegents<-function(x,y,legends,colors){
  size = length(legends)
  for (i in 1:size ){
    xCoords = c(x+0.1,x+3-0.1,x+3-0.1,x+0.1)
    yCoords = c(y-0.1,y-0.1,y+1+0.1,y+1+0.1)
    
    polygon(xCoords,yCoords,col = colors[i+1],border = NA)
    text(x+20,y+0.5,legends[i],cex = 0.4)
    
    y = y-1.5    
  }
}

putButtomLegend<-function(xStart,y){
  xStart = xStart + 3

  xCoords = c(xStart+0.1,xStart+3-0.1,xStart+3-0.1,xStart+0.1)
  yCoords = c(y-0.1,y-0.1,y+1+0.1,y+1+0.1)
  
  polygon(xCoords,yCoords,col = 'pink',border = NA)
  text(xStart+10,y+0.5,"C>A",cex = 0.4)
  
  y=-2.1
  xCoords = c(xStart+0.1,xStart+3-0.1,xStart+3-0.1,xStart+0.1)
  yCoords = c(y-0.1,y-0.1,y+1+0.1,y+1+0.1)
  
  polygon(xCoords,yCoords,col = 'orange1',border = NA)
  text(xStart+10,y+0.5,"C>T",cex = 0.4)

  y=-3.2
  xCoords = c(xStart+0.1,xStart+3-0.1,xStart+3-0.1,xStart+0.1)
  yCoords = c(y-0.1,y-0.1,y+1+0.1,y+1+0.1)
  
  polygon(xCoords,yCoords,col = 'cyan',border = NA)
  text(xStart+10,y+0.5,"C>G",cex = 0.4)
}

drawHeatmap<-function (heatData,xLabel,yLabel,rightBar,topBar,buttomBar,variants){
  sampleSize = ncol(heatData)
  xSize <- ncol(heatData)
  ySize <- nrow(heatData)
  maxColorSize = 10
  plot (c(-10,xSize+40),
        c(-30,ySize+20),
        type= 'n',
        axes = FALSE, 
        ann=FALSE)
  colors = c('gray','blue','purple','orange','green','red','black','yellow')
  # loops for create heatmap using polygon statement
  for(i in (ySize+1):2){
    for (j in 1:xSize){
      x = heatData[i-1,j]
      xCoords = c(j,j+1,j+1,j)
      yCoords = c(i-0.01,i-0.01,i-1+0.01,i-1+0.01)
      polygon(xCoords,yCoords,
              col = colors[x+1],
              border = NA)
    }
    text(-6,i-0.5,yLabel[i-1],cex = 0.4)
    percentageValue = calcSum(rightBar[i-1,])/sampleSize
    text(j+5,i-0.5,paste(round(percentageValue*100),"%"),cex = 0.4)
    drawRightBoxPlot(i,j+10,rightBar[i-1,],max(rightBar))
  }
  
  xCoords = c(xSize+10,xSize+10,xSize+40,xSize+40)
  yCoords = c(ySize+3,ySize+2,ySize+2,ySize+3)
  lines(xCoords,yCoords,type='l')
  text(xSize+25,ySize+3,max(rightBar),cex=0.4)

  for (j in 1:xSize){
    drawTopBoxPlot(ySize+2,j+1,topBar[j,],max(topBar))
    drawButtonBoxPlot(-3,j+1,buttomBar[j,])
  }
  putButtomLegend(xSize,-1)
  putLegents(0,-5,variants,colors)
}

getFirstZeroColumn<-function(array,colStart){
  len = length(array)
  for (i in 1: len){
    if (i>=colStart && array[i]==0){
      return (i)
    }
  }
  return (-1)
}

sortMatrix <-function(dataMatrix){
    sortedMatrix = dataMatrix
    i=1
    colSize = ncol(sortedMatrix)
    startColumn = 1
    zeroColumn = 0
    stackedIndex = c()

    while ( i<(nrow(sortedMatrix)) ){
      if (i==1){
        sortedIndex = order(sortedMatrix[i,1:colSize],decreasing = TRUE)
        sortedMatrix = sortedMatrix[,sortedIndex]
        zeroColumn = getFirstZeroColumn(sortedMatrix[i,],1)
        stackedIndex = c(stackedIndex,1:(zeroColumn-1))
      }
      else
      {
        sortedIndex = order(sortedMatrix[i,zeroColumn:colSize],decreasing = TRUE) + (zeroColumn-1)
        tempIndex = c(stackedIndex,sortedIndex)
        sortedMatrix = sortedMatrix[,tempIndex]
        zeroColumn = getFirstZeroColumn(sortedMatrix[i,],zeroColumn)
        stackedIndex = c(1:zeroColumn-1)
      }    
      #print (i)
      i=i+1
    }
    return (sortedMatrix)
}

MyOncoplot<-function(lamf){
  lamfData = lamf@data
  hugoSymbols = getOrderedUniqueHugoNames(lamfData$Hugo_Symbol)
  samples = getOrderedUniqueSamples(lamfData$Tumor_Sample_Barcode)
  variants = getUniqueVariants(lamfData$Variant_Classification)
  
  rowSize = nrow(hugoSymbols)
  colSize = nrow(samples)
  variantSize = nrow(variants)
  
  dataMatrix = matrix(0,rowSize,colSize)
  rightBar = matrix(0,rowSize,variantSize)
  topBar = matrix(0,colSize,variantSize)
  buttonBar =matrix(0,colSize,3)
  
  lamfDataSize = nrow(lamfData)
  gene_count =0
  
  for (i in 1:lamfDataSize){
    myRow = lamfData[i,]
    index_hugo = which(hugoSymbols$hugoNames == myRow$Hugo_Symbol)
    index_sample = which(samples$samples == myRow$Tumor_Sample_Barcode)
    index_variant = which(variants$variant == myRow$Variant_Classification)
    tumor_sample_Alelle = myRow$Tumor_Seq_Allele2
    referenceAllele = myRow$Reference_Allele
    
    if (length(index_hugo)>0 && length(index_sample)>0  && length(index_variant)>0){
      if (dataMatrix[index_hugo,index_sample]!=0){
        if (index_hugo==1){
          print (paste("duplicate",index_sample,"_",index_variant))
        }
      }
        
      dataMatrix [index_hugo,index_sample] = index_variant
      if (tumor_sample_Alelle=="C"){
        if (referenceAllele == "T"){
          buttonBar[index_sample,1] = buttonBar[index_sample,1]+1
        }else if (referenceAllele == "G"){
          buttonBar[index_sample,2] = buttonBar[index_sample,2]+1
        }else if (referenceAllele == "A"){
          buttonBar[index_sample,3] = buttonBar[index_sample,3]+1
        }
      }
    }    
  }
  
  print(paste("Gene Count",gene_count))
  sumOfRows = rowSums(dataMatrix>0)
  sortedIndex = order(sumOfRows,decreasing = TRUE)

  dataMatrix = dataMatrix[sortedIndex,]
  newSum = rowSums(dataMatrix>0)
  
  dataMatrix = sortMatrix(dataMatrix)
  zeroColumnIndex = which(colSums(dataMatrix) != 0 )
  dataMatrix = dataMatrix[, zeroColumnIndex ]
  topBar = topBar[zeroColumnIndex,]
  buttonBar = buttonBar[zeroColumnIndex,]
  rowSize = nrow(dataMatrix)
  colSize = ncol(dataMatrix)

  for (i in 1:rowSize){
    for (j in 1:colSize){
      index_variant = dataMatrix[i,j]
      rightBar[i,index_variant] = rightBar[i,index_variant] + 1 
      topBar[j,index_variant] = topBar[j,index_variant] + 1 
    }
  }
  
  
  return (list("data"=dataMatrix, 
               "yLabel" = c(hugoSymbols$hugoNames[sortedIndex]),
               "rightBar"= rightBar,
               "xLabel" = c(samples$samples),
               "topBar" = topBar,
               "buttonBar" = buttonBar,
               "variants"=c(variants$variants)))
}

#path to TCGA LAML MAF file
laml.maf = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools')
#clinical information containing survival information and histology. This is optional
#laml.clin = system.file('extdata', 'tcga_laml_annot.tsv', package = 'maftools')

laml = read.maf(maf = laml.maf,
                #clinicalData = laml.clin,
                verbose = FALSE)
#head(laml@data)
#oncoplot(laml@data)
result = MyOncoplot(laml)
pdf('result.pdf')
drawHeatmap(result$data, 
            result$xLabel,
            result$yLabel,
            result$rightBar,
            result$topBar,
            result$buttonBar,
            result$variants)
dev.off()
