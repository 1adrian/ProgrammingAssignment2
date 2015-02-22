## Here is a pair of functions that cache the inverse of a INVERTIBLE matrix
## to test them you might use mym <- rbind(c(1, -1/4), c(-1/4, 1)), an invertible matrix
##it is an adaptation of the "vector" example
## presented at https://github.com/rdpeng/ProgrammingAssignment2

## makeCacheMatrix creates a special "mmatrix", a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL ##each time the matrix is set, the cached inverse matrix becomes NULL
  }
  getMatrix <- function() x # return the current matrix
  setInverse <- function(solve) inv <<- solve  
  getInverse <- function() inv
  list(set = setMatrix, get = getMatrix,
       seti = setInverse,
       geti = getInverse)
}


##cacheSolve calculates the inverse of the special "matrix" created with the above function. 
cacheSolve <- function(x, ...) {
  inv <- x$geti()
  if(!is.null(inv)) {  ## if the inverse is not NULL, skip the tedious computation
    message("getting cached the inverse matrix")
    return(inv)
  }
  theMatrix <- x$get() #get the new/modified matrix
  inv <- solve(theMatrix, ...)  
  x$seti(inv) 
  inv # return the new inverse matrix
  
}
