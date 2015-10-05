pollutantmean <- function(directory, pollutant, id = 1:332) {
  theMean = 0
  theSum = 0
  theCnt = 0
  theFile <- list.files(directory)
  for (i in 1:length(id)) {
    theData <- read.csv(paste(directory, "/", theFile[id[i]], sep = ""))
    theSum = theSum + sum(theData[,pollutant], na.rm=TRUE)
    theCnt = theCnt + length(which(!is.na(theData[,pollutant])))
  }
  theSum/theCnt
}