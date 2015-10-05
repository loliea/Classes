rankall <- function(outcome, num = "best") {
  ## Read outcome data
  theOutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Convert the content of the 3 target columns in numeric; desable warnings as NA will be introduce due to existing NA
  options(warn = -1)
  theOutcome[,11] <- as.numeric(theOutcome[,11])
  theOutcome[,17] <- as.numeric(theOutcome[,17])
  theOutcome[,23] <- as.numeric(theOutcome[,23])
  options(warn = 1)
  
  ## Check that the outcome is valid
  if (!(outcome %in% c("heart attack", "heart failure","pneumonia"))) stop("invalide outcome")
  
  
  ## Subset the dataset to only keep the data for the column needed
  ## Hospital, State, 30 days death  for each of the three outcomes
  theOutcome_state <- theOutcome[,c(2,7,11,17,23)]
  ## If it outcome is valid then extract the column number of the dataset
  if (outcome == "heart attack") theCol <- 3 else if (outcome == "heart failure") theCol <- 4 else theCol <- 5
  
  ##Mega sort by state, outcome selected and hospital name
  theSort <- order(theOutcome_state[,2], theOutcome_state[,theCol], theOutcome_state[,1], na.last = NA)
  theOutcome_state_sort <- theOutcome_state[theSort,c(1, 2, theCol)]
  
  theOutcome_split_state <- split(x = theOutcome_state_sort, f = theOutcome_state_sort$State)
  
  ## For each state, find the hospital of the given rank
  ## create empty data frame that will contain the result
  rank_all <- data.frame(hospital = as.character(), state = as.character())
  #To apply to each state; here for example only to ID
  ## best
  if (num == "best") {
    for (i in 1:54) {
      rank_all <- rbind(rank_all, data.frame(hospital = theOutcome_split_state[[i]][1, 1], 
                                             state = theOutcome_split_state[[i]][1, 2],
                                             row.names = names(theOutcome_split_state[i])))
    }
  } 
  ## worst
  else if (num == "worst") {
    for (i in 1:54) {
      rank_all <- rbind(rank_all, data.frame(hospital = theOutcome_split_state[[i]][nrow(theOutcome_split_state[[i]]), 1], 
                                             state = theOutcome_split_state[[i]][nrow(theOutcome_split_state[[i]]), 2],
                                             row.names = names(theOutcome_split_state[i])))
    }
  }
  
  ## use the value if num
  else {
    for (i in 1:54) {
      rank_all <- rbind(rank_all, data.frame(hospital = theOutcome_split_state[[i]][num, 1], 
                                                            state = names(theOutcome_split_state[i]),
                                                            row.names = names(theOutcome_split_state[i])))
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
rank_all
  
}