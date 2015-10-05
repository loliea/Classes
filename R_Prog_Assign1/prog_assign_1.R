pollutantmean <- function(directory, pollutant, id = 1:332) {
  theMean = 0
  theSum = 0
  theCnt = 0
  theFile <- list.files()
  for (i in id) {
    #theData <- read.csv(paste(substr(as.character(paste("00",i,sep="")),nchar(i),2+nchar(i)),".csv", sep=""))
    theData <- read.csv(theFile[i])
    theSum = theSum + sum(theData[,pollutant], na.rm=TRUE)
    theCnt = theCnt + length(which(!is.na(theData[,pollutant])))
  }
  print(theSum/theCnt)
}

complete <- function(directory, id = 1:332) {
  theCnt = 0
  theFile <- list.files()
  #tempDataID
  outFrame <- data.frame(id = 1, nobs = 1)
  for (i in 1:length(id)) {
    theData <- read.csv(theFile[id[i]])
    #print(head(theData))
    theCnt <- length(which(!is.na(theData[,1])&!is.na(theData[,2])&!is.na(theData[,3])))
    outFrame[i,] <- data.frame(id = id[i], nobs = theCnt)
    #tempDataID[i] <- i
    #tempDataCnt[i] <- theCnt
  }
  #outFrame <- data.frame(id = tempDataID, nobs = tempDataCnt)
  outFrame
}

corr <- function(directory, threshold = 0) {
  theFile <- list.files(directory)
  theCorr = 0
  outCorr <- data.frame(id = 0, Corr = 0)
  outComplete <- complete(directory)
  j = 1
 for (i in 1:nrow(outComplete)) {
   if (outComplete[i,2] > threshold) {
     theFileSelect <- read.csv(paste(directory, "/", theFile[i], sep = ""))
     theCorr <- cor(theFileSelect[!is.na(theFileSelect[,2])&!is.na(theFileSelect[,3]),2], theFileSelect[!is.na(theFileSelect[,2])&!is.na(theFileSelect[,3]),3])
     outCorr[j,] <- data.frame(id = i, Corr = theCorr)
     j = j + 1
   }
 }
 if (outCorr[1,1] == 0) {
   NA
 }
 else outCorr[,2]
 #If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0
}