#best function that list the hospital with the lowest rate for the state and outcome entered.
#arguments: state (2 characters) and condition (can be “heart attack”, “heart failure”, or “pneumonia”)
#if argument is incorrect then should return the appropriate error message via the stop function "invalid state" or "invalide outcome".
best <- function(state, outcome) {
  #reads the content of the file and store it under data frame named theOutcome
  theOutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #convert the content of the 3 target columns in numeric; desable warnings as NA will be introduce due to existing NA
  options(warn = -1)
  theOutcome[,11] <- as.numeric(theOutcome[,11])
  theOutcome[,17] <- as.numeric(theOutcome[,17])
  theOutcome[,23] <- as.numeric(theOutcome[,23])
  options(warn = 1)
  
  #Reads name of the state entered and check if it exists
  #If it does its position will be save in logical vector state_t otherwise will stop execution
  theRows <- theOutcome[,7] == state  #theRows capture the lines where the state is the one entered
  if (is.na(table(theRows)["TRUE"])) stop("invalid state")
  
  #Subset the dataset to only keep the data for the selected state
  theOutcome_state <- theOutcome[theRows,c(2,7,11,17,23)]
  
  #Reads outcome entered and check if it exists
  #“heart attack”, “heart failure”, or “pneumonia”
  if (!(outcome %in% c("heart attack", "heart failure","pneumonia"))) stop("invalid outcome")
  #If it exist then extract the column number of the dataset
  if (outcome == "heart attack") theCol <- 3 else if (outcome == "heart failure") theCol <- 4 else theCol <- 5
  
  #Look for min
  theMin <- min(theOutcome_state[,theCol], na.rm = TRUE)
  #Select the range of rows where the lowest value is theMin excluding the NA
  theMin_range <- theOutcome_state[,theCol] == theMin & !is.na(theOutcome_state[,theCol])
  
  #Match lowest rate to the corresponding hospitals
 #print(theOutcome_state[theMin_range, c(1, 2, theCol)])
  head(sort(theOutcome_state[theMin_range, 1]), 1)
}