rankhospital <- function(state, outcome, num = "best") {
  ## Reads the content of the file and store it under data frame named theOutcome
  theOutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Convert the content of the 3 target columns in numeric; desable warnings as NA will be introduce due to existing NA
  options(warn = -1)
  theOutcome[,11] <- as.numeric(theOutcome[,11])
  theOutcome[,17] <- as.numeric(theOutcome[,17])
  theOutcome[,23] <- as.numeric(theOutcome[,23])
  options(warn = 1)
  
  ## Reads name of the state entered and check if it exists
  ## If it does its position will be save in logical vector state_t otherwise will stop execution
  theRows <- theOutcome[,7] == state  #theRows capture the lines where the state is the one entered
  if (is.na(table(theRows)["TRUE"])) stop("invalid state")
  
  ## Subset the dataset to only keep the data for the selected state and the column needed
  ## Hospital, State, 30 days death  for each of the three outcomes
  theOutcome_state <- theOutcome[theRows,c(2,7,11,17,23)]
  
  ## Reads outcome entered and check if it exists
  ## Should be “heart attack”, “heart failure”, or “pneumonia” otherwise stops
  if (!(outcome %in% c("heart attack", "heart failure","pneumonia"))) stop("invalid outcome")
  ## If it outcome is valid then extract the column number of the dataset
  if (outcome == "heart attack") theCol <- 3 else if (outcome == "heart failure") theCol <- 4 else theCol <- 5
  
  
  ## Order the data frame theOutcome_state by the column of the outcome and then the name of the hospital in case of ties
  theOutcome_state_order <- theOutcome_state[order(theOutcome_state[,theCol], theOutcome_state[,1], na.last = NA),]

  ## Test the value entered  for num and associate it to the correct index to be returned and then print the result
  if (is.numeric(num) & num >nrow(theOutcome_state_order)) {
    print(NA)
  } else {
    if (num == "best") {
      theNum = 1
    } else if (num == "worst") {
      theNum = nrow(theOutcome_state_order)
    } else theNum = num
    print(theOutcome_state_order[theNum, 1])
  }

}

#rankhospital("TX", "heart failure", "worst")