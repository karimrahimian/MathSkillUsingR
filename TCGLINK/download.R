library(TCGAbiolinks)
library(SummarizedExperiment)
library(dplyr)
library(DT)
library(maftools)

createFolder<-function(cancerNames,rootDir){
  for (cancer in cancerNames){
    inputPathMaf <- paste(rootDir,"/inputs/maf/",cancer,sep="")
    inputPathRData  <- paste(rootDir,"/inputs/rdata/",cancer,sep="")
    outputPath<- paste(rootDir,"/outputs/",cancer,sep="")
    
    if (!dir.exists(inputPathMaf)){
      dir.create(inputPath,recursive = TRUE)
    }
    if (!dir.exists(outputPath)){
      dir.create(outputPath,recursive = TRUE)
    }

    if(!dir.exists(inputPathRData)){
      dir.create(inputPathRData,recursive = TRUE)
    }
  }
}
getCurrentDirectory<-function(){
  parentDirName <-dirname(rstudioapi::getActiveDocumentContext()$path)
  return (parentDirName)
}
downloadTCGA<-function(cancer,rootDir){
  query_snp <- GDCquery(project = paste0("TCGA-",cancer),
                        data.category = "Simple Nucleotide Variation",
                        data.type = "Masked Somatic Mutation",
                        workflow.type = "Aliquot Ensemble Somatic Variant Merging and Masking")
  GDCdownload(query_snp, files.per.chunk = 100,directory = paste0(rootDir,"/inputs/maf/",cancer))
}
downloadTCGAList<-function(cancerNames){
  rootDir <- getCurrentDirectory()
  for (cancer in cancerNames){
    downloadTCGA(cancer,rootDir )
  }
}
getExtension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 
loadMAFRecursive <-function(cancer,rootDir){
    suffixPath = paste0("/TCGA-",cancer,"/harmonized/Simple_Nucleotide_Variation/Masked_Somatic_Mutation/")
    mafFolderPath = paste0(rootDir,"/inputs/maf/",cancer,suffixPath)
    mafList = c()
    folders =list.dirs(mafFolderPath,full.names = TRUE)[-1]
    for (folder in folders){
      mafFiles = list.files(folder)
      for (mafFile in mafFiles){
          fullMafFileName = paste0(folder,"/",mafFile)
          print(fullMafFileName)
          esult = tryCatch({
            laml <- read.maf(maf =fullMafFileName, useAll = T, verbose = T)
            mafList = c(mafList,laml)
          }, warning = function(w) {
            print(w)
          }, error = function(e) {
            print(e)
          }, finally = {}
          )
      }
    }
    return (mafList)
}
loadMAF <-function(cancer,rootDir){
  suffixPath = "/TCGA-ACC/harmonized/Simple_Nucleotide_Variation/Masked_Somatic_Mutation/"
  suffixPath = ""
  mafFolderName = paste0(rootDir,"/inputs/maf/",cancer,suffixPath)
  mafFiles = list.files(mafFolderName)[1]
  fullMafFileName = paste0(mafFolderName,"/",mafFiles)
  
  laml.maf <- system.file(fullMafFileName, package = "maftools")
  laml <- read.maf(maf = laml.maf)
  return (laml)
}
saveAsRData<-function(mafObject,cancer,rootDir){
  outputPath = paste0(rootDir,"/inputs/rdata/",cancer,"/",cancer,".rds")
  saveRDS(mafObject,outputPath)
}
drawOncoplot<-function(cancer){
  rootDir = getCurrentDirectory()
  rdataPath = paste0(rootDir,"/inputs/rdata/",cancer,"/",cancer,".rds")
  mafData = readRDS(rdataPath)
  laml = mafData
  
  outputPathPdf = paste0(rootDir,"/outputs/",cancer,"/",
                         "OncoPlot.pdf")
  pdf(outputPathPdf)
  oncoplot(maf = laml, draw_titv = TRUE)
  plotmafSummary(maf = laml, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)
  dev.off()
 
  outputPathPdf = paste0(rootDir,"/outputs/",cancer,"/","SummaryPlot.pdf")
  pdf(outputPathPdf)
  plotmafSummary(maf = laml, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)
  dev.off()
  
  outputPathPdf = paste0(rootDir,"/outputs/",cancer,"/", "pivotPlot.pdf")
  pdf(outputPathPdf)
  laml.titv = titv(maf = laml, plot = FALSE, useSyn = TRUE)
  plotTiTv(res = laml.titv)
  dev.off()
  
    
  outputPathPNG = paste0(rootDir,"/outputs/",cancer,"/",cancer , "png")
  
  png(outputPathPNG)
  oncoplot(maf = laml, draw_titv = TRUE)
  dev.off()
}

drawPlot <- function (cancer,laml,rootDir){
  pdfPath <- paste0(rootDir,"/outputs/maf/",cancer,"/ACC.PDF")
  pdf(file = "/Users/ndphillips/Desktop/My Plot.pdf",   width = 4, # The width of the plot in inches
      height = 4) #
  lollipopPlot( maf = laml, gene = 'DNMT3A',   AACol = 'Protein_Change',  showMutationRate = TRUE,
    labelPos = 882
  )
  dev.off()
}
mergeMafAndConvertToRData<-function(cancerNames){
  for (cancer in cancerNames){
    splitMafs = loadMAFRecursive(cancer,rootDir)
    mergerdMaf = merge_mafs(splitMafs)
    saveAsRData(mergerdMaf,cancer,rootDir)
  }
}
initialSettings<-function(cancerNames){
  
  rootDir <- getCurrentDirectory()
  setwd(rootDir)
  createFolder(cancerNames, rootDir)
}

cancerNames = c("ACC","BRCA","SARC")
initialSettings(cancerNames)
drawOncoplot("ACC")

