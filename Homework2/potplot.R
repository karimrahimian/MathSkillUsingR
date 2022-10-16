library(maftools)

getOrderedUniqueHugoNames<-function(hugoNames){
  uniqueHugoNames = data.frame(table(hugoNames))
  filteredHugoNamesIndex= which(uniqueHugoNames$Freq>=5)
  filteredUniqueHugoNames = uniqueHugoNames[filteredHugoNamesIndex,]
  orderedUniqueHugoName = filteredUniqueHugoNames[order(filteredUniqueHugoNames$Freq,decreasing = FALSE),]
  return (orderedUniqueHugoName)
}

getOrderedUniqueSamples<-function(samples){
  uniqueSamples = data.frame(table(samples))
  filteredUniqueSamplesIndex= which(uniqueSamples$Freq>=2)
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
  colors = c('blue','beige','orange','green','red','black','yellow')
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
  colors = c('blue','beige','orange','green','red','black','yellow')
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
  colors = c('blue','beige','orange','green','red','black','yellow')
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

drawHeatmap<-function (heatData,xLabel,yLabel,rightBar,topBar,variants){
  sampleSize = ncol(heatData)
  xSize <- ncol(heatData)
  ySize <- nrow(heatData)
  maxColorSize = 10
  plot (c(-10,xSize+40),
        c(-30,ySize+20),
        type= 'n',
        axes = TRUE, 
        ann=FALSE)
  colors = c('gray','blue','beige','orange','green','red','black','yellow')
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
    

  for (j in 1:xSize){
    drawTopBoxPlot(ySize+2,j+1,topBar[j,],max(topBar))
    drawButtonBoxPlot(-3,j+1,topBar[j,])
  }
  putLegents(0,-5,variants,colors)
  
}



  


oncoplot12<-function(lamf){
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
  buttonBar =matrix(0,colSize,variantSize)
  
  lamfDataSize = nrow(lamfData)
  for (i in 1:lamfDataSize){
    row = lamfData[i,]
    index_hugo = which(hugoSymbols$hugoNames == row$Hugo_Symbol)
    index_sample = which(samples$samples == row$Tumor_Sample_Barcode)
    index_variant = which(variants$variant == row$Variant_Classification)
    
    if (length(index_hugo)>0){
      dataMatrix [index_hugo,index_sample] = index_variant
      rightBar[index_hugo,index_variant] = rightBar[index_hugo,index_variant] + 1 
      topBar[index_sample,index_variant] = topBar[index_sample,index_variant] + 1 
      buttonBar[index_sample,index_variant] = buttonBar[index_sample,index_variant] + 1 
    }    
    
  }
  return (list("data"=dataMatrix, 
               "yLabel" = c(hugoSymbols$hugoNames),
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
result = oncoplot12(laml)
pdf('result.pdf')
drawHeatmap(result$data, 
            result$xLabel,
            result$yLabel,
            result$rightBar,
            result$topBar,
            result$variants)
dev.off()
