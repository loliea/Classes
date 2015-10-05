setwd("./GitHub/WorkWithData/Week1")

DT <- fread("Q5DataSet.csv")

twoMeans <- replicate(1000, system.time({mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)}))[1,]
teeApply <- replicate(1000, system.time(tapply(DT$pwgtp15,DT$SEX,mean)))[1,]
dataTable <- replicate(1000, system.time(DT[,mean(pwgtp15),by=SEX]))[1,]
EssApply <- replicate(1000,system.time(sapply(split(DT$pwgtp15,DT$SEX),mean)))[1,]

plot(EssApply, type = "l", col="#FF000099")
lines(dataTable, type = "l", col="#0000FF99")

av_EssApply <- cumsum(EssApply)/seq_along(EssApply)
av_dataTable <- cumsum(dataTable)/seq_along(dataTable)

plot(av_dataTable, type = "l", col="#0000FF99")
lines(av_EssApply, type = "l", col="#FF000099")