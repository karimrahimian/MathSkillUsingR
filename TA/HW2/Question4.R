heartData <- read.csv(file = 'heart.csv',
                      header = FALSE,
                      sep = " ",
                      )
filterdHeartData = heartData[,c(1:5)]
colnames(filterdHeartData) <- c('Age','Sex','Cp','TrestBPS','Chol')
selectedCholestrols <- filterdHeartData[filterdHeartData$TrestBPS>120,]
averageAge = mean(selectedCholestrols$Age)
print (paste("Average Age= ",averageAge))
pdf("Heart.pdf")
boxplot(Age ~ Sex, 
        data = selectedCholestrols, 
        ylab = "Sex",
        xlab = "Age", 
        main = "Heart Disease",
        names=c('Men','Women'),
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
dev.off()
