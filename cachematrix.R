## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix2 <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(m) m <<- m
  getm <- function() m
  list(set = set, get = get,
       setm = setm,
       getm = getm)
}


## Write a short comment describing this function

cacheSolve2 <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setm(m)
  m
  
}
