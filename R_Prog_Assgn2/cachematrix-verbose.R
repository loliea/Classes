## Elie Amaraggi - 5/19/15
## This program stores a matrix and its computed inverse in a list object.
## If the inverse of a matrix has already been stored with the corresponding matrix then it will be retrieved instead of being calculated.
## This program contains two functions:
##    - makeCacheMatrix: create a list object that will contain vector functions that will store the matrix and its associated inverse
##    - cacheSolve: Will ether calculate the inverse of the matrix and store it, or will retrieve its value if it already exists.

## Function makeCachematrix:
## This function generate a list object that will store the value of the matrix itself as well as its inverse via four vector functions:
##  - setMat: set the value of the matrix
##  - getMat: retrieve the value of the matrix
##  - setInv: set the value of the inverse
##  - getMat: retrieve the value of the inverse
## This function also tests wheter the matrix is a squarre matrix as only these are inversible

makeCacheMatrix <- function(x = matrix()) {
##Initialize the parameters of the list that is going to store the matrix and its inverse
  theInv <- NULL
  setMat <- function(y) { ##function that sets the value of the matrix
    x <<- y
    theInv <<- NULL
  }
  getMat <- function() x ##function that retrieve the value of the matrix
  setInv <- function(Inv) {theInv <<- Inv} ##function that sets the value of the inverse (Inv)
  getInv <- function() theInv ##function that retrieves the value the inverse of the matrix
  list(getMat = getMat, setMat = setMat, getInv = getInv, setInv = setInv) #list object that stores the functions associated with the matrix
}


## Function cacheSolve:
## This function computes the inverse of the matrix of the object (stored in getMat) if it doesn' t exist.
## If it exists then it will return its value directly
cacheSolve <- function(x, ...) {
  if (dim(x$getMat())[1] != dim(x$getMat())[2]) {
    message("Matrix is not square so cannot compute its inverse.")
  }
  ## Part 1
  ## In this part of the function we test whether the value of the inverse of the object already exist
  else {
    Inv <- x$getInv() ##Acquire the content getInv of the object x
    if(!is.null(Inv)) { ##Test if it is not null
     message("Inverse has already been computed, and it is:") ##If not null then print the message...
     return(Inv) ##... return its value
   }
## End Part 1
##Part 2
## In this part we will calculate the value of the inverse of the object matrix element of x
    mat <- x$getMat() ## retrieve the value of the matrix element of x
## TO DO Will need to have the variable in diag dynamic based on the dim of x
   d <- dim(mat)[1] ## Get the dimensionality of the input matrix to resolve dynamically its inverse
   Inv <- solve(mat,diag(d)) ## calculate the inverse of the matrix of the object x and store the output in Inv
   x$setInv(Inv) ## Call the function setInv for the list object x (which is the argument of the cacheSovle function) with the value of Inv to set the value of the inverse of the object x
## End of part 2
  Inv ##just print the actual value of the inverse of the matrix
  }
}
