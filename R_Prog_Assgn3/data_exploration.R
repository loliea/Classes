#Download the file
download.file(url = "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip", destfile ="/Users/loliea/GitHub/ProgAssign3/ProgAssignment3-data.zip", method = "libcurl")

#look at the content fo the zip file
unzip(zipfile = "ProgAssignment3-data.zip", list = TRUE)
#unzip it in working directory
unzip(zipfile = "ProgAssignment3-data.zip")
#Upload one of the csv to otucome data frame
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
dim(outcome)  #check number of rows and columns of outcome
names(outcome)  #look at the headers of the data frame outcome
str(outcome)
