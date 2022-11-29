extractOrganismSequence<-function(fileName,
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
  result = c()
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
        if (foundResultOranism==TRUE && foundResultRNA == TRUE){
          result = c(result,headLine)
          result = c(result,sequence)
          k=k+1
          if (k>=sequenceLimit){
            break
          }
        }
    }
    i = i+2
  }
  return (result)
}
writeSequenceInFile<-function(fileName,rnaList){
  fileOutput <- file(fileName)
  writeLines(rnaList,fileOutput)
  close(fileOutput)
}

extractedHumanRNA = extractOrganismSequence('ncrna_NONCODE_v3.fasta',
                                            seekingOrganism="Homo sapiens")
writeSequenceInFile('HomoSapiens_RNA.fasta',extractedHumanRNA)



