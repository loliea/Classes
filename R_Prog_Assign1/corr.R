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
    c()
  }
  else outCorr[,2]
}