a <- outcome[,7] == "CA"
if (is.na(table(a)["TRUE"])) {
  stop("invalide state")
}

theoutcome <- "heart attack"
if (!(theoutcome %in% c("heart attack", "heart failure","pneumonia"))) print("notok") else print("ok")

Rows <- outcome[,7] == "TX"
outcome_state <- outcome[,c(2,7,11,17,23)]

match <- outcome_state[,5] == 6.8 & !is.na(outcome_state[,5])

head(outcome_state[order(outcome_state[,5]),5])


best("CA","pneumonia")
best("FL","heart failure")
best("AZ","heart attack")

#count rate for each outcome by states
tapply(X = outcome[,11], INDEX = outcome$State, function(x) head(table(x), 5)) #"heart attack"
tapply(X = outcome[,17], INDEX = outcome$State, function(x) head(table(x), 1)) # "heart failure" -->MS, SD, WI have 2
tapply(X = outcome[,23], INDEX = outcome$State, function(x) head(table(x), 1)) # "pneumonia-->MA, UT have 2


tCol <- 4 ##"heart attack" 3 || "heart failure" 4 || "pneumonia" 5
##                          STATE           OUTCOME            HOSPITAL
Tsort <- order(outcome_state[,2], outcome_state[,tCol], outcome_state[,1], na.last = NA) 
outcome_state_sort <- outcome_state[Tsort,]
split_state <- split(x = outcome_state_sort, f = outcome_state_sort$State)

#To apply to each state; here for example only to ID
##best
split_state[[15]][1, c(1,2,tCol)]
##worst
split_state[[15]][nrow(split_state[[15]]), c(1,2,tCol)]
## if doesn't exist
thenum <- 26
if (thenum > nrow(split_state[[15]])) {
  print(c(NA, names(split_state)[15], NA))
} else split_state[[15]][thenum, c(1,2,tCol)]


## For each state sort by outcome and hosto
#test <- order(split_state[[1]][11], split_state[[1]][2])
#cbind(split_state[[1]][2][test,1], split_state[[1]][11][test,1])



mean(outcome[outcome[,7] == "AL", 11], na.rm = TRUE)

(outcome == "heart attack") theCol <- 3 else if (outcome == "heart failure") theCol <- 4 else theCol <- 5

############################## SECOND ###########################
#the meat
a <- outcome[,7] == "TX"
outcome_state <- outcome[a,c(2,7,11,17,23)]
theCol <- 4 #"heart attack") <- 3 else "heart failure"<- 4 else 5
order_outcome_state <- outcome_state[order(outcome_state[,theCol],outcome_state[,1], na.last = NA) ,]

Rows = 12
if (Rows == "best") numRows = 1 else if (Rows == "worst") numRows = length(order_outcome_state) else numRows = Rows
print(order_outcome_state[numRows,1])
c_order_outcome_state <- cbind(order_outcome_state, rank(order_outcome_state[,theCol], ties.method = "first"))
if (Rows == "best") {
  print(c_order_outcome_state[numRows:(numRows+5),c(1,2,theCol,6)])
  } else if (Rows == "worst") {
    print(c_order_outcome_state[(numRows-5):numRows,c(1,2,theCol,6)])
  } else print(c_order_outcome_state[(numRows-5):(numRows+5),c(1,2,theCol, 6)])
#end the meat
#The num argument can take values “best”, “worst”, or an integer indicating the ranking
#(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
#state, then the function should return NA. Hospitals that do not have data on a particular outcome should
#be excluded from the set of hospitals when deciding the rankings.

rankhospital("MN", "heart attack", 6)
rankhospital("TX", "heart failure", 6)


n = c(2, 3, 5) 
s = c("aa", "bb", "cc", "dd", "ee") 
b = c(TRUE, FALSE, TRUE, FALSE, FALSE) 
x = list(n, s, b, 3)   # x contains copies of n, s, b