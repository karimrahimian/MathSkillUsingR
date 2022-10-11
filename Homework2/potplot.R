getFilteredUniqueHugoNames<-function(hugoNames){
  uniqueHugoNames = data.frame(table(hugoNames))
  filteredHugoNamesIndex= which(uniqueHugoNames$Freq>5)
  filteredUniqueHugoNames = uniqueHugoNames[filteredHugoNamesIndex,]
}

oncoplot<-function(lamfData){
  filteredUniqueHugoSymbol = getFilteredUniqueHugoNames(lamfData$Hugo_Symbol)
  print(filteredUniqueHugoSymbol)
  
}

library(maftools)
#path to TCGA LAML MAF file
laml.maf = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools')
#clinical information containing survival information and histology. This is optional
#laml.clin = system.file('extdata', 'tcga_laml_annot.tsv', package = 'maftools')

laml = read.maf(maf = laml.maf,
                #clinicalData = laml.clin,
                verbose = FALSE)

#head(laml@data)
#oncoplot(laml@data)
oncoplot(maf = laml, draw_titv = TRUE)
