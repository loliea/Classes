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
  theInv <- NULL
  setMat <- function(y) {
    x <<- y
    theInv <<- NULL
  }
  getMat <- function() x
  setInv <- function(Inv) {theInv <<- Inv}
  getInv <- function() theInv
  list(getMat = getMat, setMat = setMat, getInv = getInv, setInv = setInv)
}

## Function cacheSolve:
## This function computes the inverse of the matrix of the object (stored in getMat) if it doesn' t exist.
## If it exists then it will return its value directly
## Note that before computing the inverse the function tests if the matrix is square or singular. If one of these tests
## is positive then a message is display with the issue and the inverse is not computed.
cacheSolve <- function(x, ...) {
  if (dim(x$getMat())[1] != dim(x$getMat())[2]) {
    message("The matrix is not square so its inverse cannot be computed.")
  }
  else if (det(x$getMat()) == 0) {
    message("The matrix is sungular and thus not inversible.")
  }
  else {
    Inv <- x$getInv()
    if(!is.null(Inv)) {
     message("Inverse has already been computed, and it is:")
     return(Inv)
   }
    mat <- x$getMat()
   d <- dim(mat)[1]
   Inv <- solve(mat,diag(d))
   x$setInv(Inv)
  Inv
  }
}
