makeVector <- function(x = numeric()) {
  m <- NULL ##Set m to Null
  set <- function(y) { ##if instead of calling the makeVector the set function is called to change the value
    x <<- y ##x gets assigned y input of the function set
    m <<- NULL ##m gets initialize to the value null
  }
  get <- function() x ##Just pass the value x to get
  setmean <- function(mean) m <<- mean ##mean is a variable of the function that returns itself
  getmean <- function() m ##If mean not calc yet, it will me Null as set at the beginning
  list(set = set,
       get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean() ##assign the value of the elment/function getmean to m
  if(!is.null(m)) {   ##if m exist then retrieves it
    message("getting cached data")
    return(m)
  }
  data <- x$get()  ## assigned the value of x to variable data
  m <- mean(data, ...)  ## calculate the mean of x and store it in m
  x$setmean(m) ##
  m
}