complete <- function(directory, id = 1:332) {
  theCnt = 0
  theFile <- list.files(directory)
  outFrame <- data.frame(id = 1, nobs = 1)
  for (i in 1:length(id)) {
    theData <- read.csv(paste(directory, "/", theFile[id[i]], sep = ""))
    theCnt <- length(which(!is.na(theData[,1])&!is.na(theData[,2])&!is.na(theData[,3])))
    outFrame[i,] <- data.frame(id = id[i], nobs = theCnt)
  }
  outFrame
}