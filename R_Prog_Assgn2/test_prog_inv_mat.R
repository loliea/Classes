test_ok <- function() {
  ##Normal 2x2 matrix generated using rnorm
  Mat2 <<- makeCacheMatrix(matrix(rnorm(4),2,2))
  #test that the value of the matrix has been set properly and its inverse is Null
  message("Printing the matrix stored")
  print(Mat2$get())
  message("Trying to retrieve the inverse which has not yet been computed so it should return Null")
  print(Mat2$getCache())
  ##Generate its inverse
  message("Calling the function cacheSolve to calculate the inverse")
  print(cacheSolve(Mat2))
  ##Call again cacheSolve function to test that it retrieves the cache
  message("Now call again the cacheSolve function and this time should retrieve the cache")
  print(cacheSolve(Mat2))
  message("Did it inform it retrieved the cache?")
}

test_error <- function() {
  ##3x3 matrix not squarre
  Mat3.1 <- makeCacheMatrix(matrix(rnorm(6),3,2))
  message("Trying to compute the invest of the matrix that is not square")
  print(cacheSolve(Mat3.1))
  message("Should have returned that the matrix is not square")

  ##3x3 matrix not inversible
  Mat3.2 <- makeCacheMatrix(matrix(1:9,3,3))
  message("Trying to computre the inverse of a singular matrix")
  print(cacheSolve(Mat3.2))
  message("Should have returned that the matrix is not inversible because singular")
}

test_replace <- function() {
  ##test the getMat function on Mat2
  message("Showing current content of Mat2")
  print(Mat2$get())
  message("Change the content of Mat2")
  Mat2$set(matrix(1:4,2,2))
  message("Retrieve the new content of Mat2")
  print(Mat2$get())
  message("Retrieve the value of the inverse of the newly changed Mat2 matrix")
  print(Mat2$getCache())
  message("Should return NULL")
  message("Now compute the inverse of the new Mat2")
  print(cacheSolve(Mat2))
  message("Print now the inverse of Mat2")
  print(Mat2$getCache())
  message("Should return the inverse of Mat2 as previously displayed")
  message("Now test that the inverse is correct")
  print(Mat2$get() %*% Mat2$getCache())
  message("Should have retruned the identity matrix")
}