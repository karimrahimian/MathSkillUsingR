getAllCodons<-function(){
  nucleotids = c("A","T","C","G")
  codons = c()
  for (i in 1:4){
    for (j in 1:4){
      for (k in 1:4){
        codon = paste(nucleotids[i],nucleotids[j],nucleotids[k],sep="") 
        codons = c(codons,codon)
      }
    }
  }
  return (table(codons))
}
readFasta<-function(fileName,skipHeader=TRUE){
  
  df = read.table(fileName,sep='\n',quote = "")
  rowSize = nrow(df)
  rnaLines = c()
  startLine = 1
  if (skipHeader == TRUE)
    startLine = 2
  for (i in startLine+1:rowSize){
    firstCharacter = substr(df$V1[i],1,1)
    if (firstCharacter != '>' ){
      rnaLines = c(rnaLines,df$V1[i])
    }
    if (i>100){
      break
    }
      
  }
  return (rnaLines)
}
getCodonFrequency<-function(rnaList){
  codon_table = data.frame(getAllCodons())
  rnaSize = length (rnaList)
  for (i in 1:rnaSize){
    rnaSequence = rnaList[i]
    rnaLength = nchar(rnaSequence)
    i=1
    while (i<=rnaLength-3){
      codon = substr(rnaSequence , i,i+2)
      index = which(codon_table$codons==codon)
      codon_table$Freq[index] = codon_table$Freq[index] +1 
      i = i+1
    }
  }
  return (codon_table)
}

mycodons = getAllCodons()

rnaList = readFasta('ncrna_NONCODE_v3.fasta')
codon_table = getCodonFrequency(rnaList)
print (codon_table)


