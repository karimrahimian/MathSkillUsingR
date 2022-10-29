extractOrganismSequenceLength<-function(fileName,
                                  skipHeader=TRUE,
                                  seekingOrganism="Homo sapiens",
                                  sequenceLimit = 300){
  df = read.table(fileName,sep='\n',quote = "")
  rowSize = nrow(df)
  rnaLines = c()
  startLine = 1
  if (skipHeader == TRUE)
    startLine = 2
  i=startLine+1
  rnaID = c()
  rnaLen = c()
  k=1
  while (i<rowSize){
    firstCharacter = substr(df$V1[i],1,1)
    if (firstCharacter == '>'){
      headLine = df$V1[i]
      sequence = df$V1[i+1]
      head = strsplit(headLine,split=" | ",fixed = TRUE)
      head = head[[1]]
      organism = head[4]
      rnaType = head[3]
      foundResultRNA = grepl('lncRNA',rnaType,fixed = TRUE)
      foundResultOranism = grepl(seekingOrganism,organism,fixed = TRUE)
      if (foundResultOranism==TRUE && foundResultRNA==TRUE){
        rnaID = c(rnaID,head[2])
        rnaLen = c(rnaLen,nchar(sequence))
        k=k+1
        if (k>=sequenceLimit){
          break
        }
      }
    }
    i = i+2
  }
  return (data.frame(rnaID,rnaLen))
}
drawBarPlot<-function(rnaList){
  rna_count = nrow(rnaList)
  ySize = 20
  maxData = max(rnaList$rnaLen)
  plot (c(-3,rna_count+1),
        c(-5,ySize),
        type= 'n',
        axes = FALSE, 
        ann=FALSE)
  
  nextStep = 1
  
  col = 1
  for (k in 1:rna_count){
    perc = ((rnaList$rnaLen[k] / maxData))*100
    yCoords = c(0,perc,perc,0)
    xCoords = c(k-0.4,k-0.4,k+0.4,k+0.4)
    polygon(xCoords,yCoords,
            col = "tomato",
            border = NA)
    text(k,-1.5,rnaList$rnaID[k],srt=-90,cex=0.1)
  }
  text(-3,9,"Frequency",srt=90)
  text(-2,0,"0-",)
  text(-2,20,"100%-",)
  text(rna_count/2-10,-5,"DNA ID")
}

extractedHumanRNA = extractOrganismSequenceLength('ncrna_NONCODE_v3.fasta',
                                            seekingOrganism="Homo sapiens")
drawBarPlot(extractedHumanRNA)


