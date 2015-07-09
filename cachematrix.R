## Put comments here that give an overall description of what your
## functions do

## Create a cacheMatrix which has functions for getting and setting
## the matrix itself and the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  getMatrix <- function() {
    x
  }

  setMatrix <-function(matrix) {
    # Check if they're identical first, only modifying matrix and inverse if
    # they are not.
    if (!identical(x, matrix)) {
      x <<- matrix
      inverse <<- NULL
    }
  }
  
  getInverse <- function() {
    inverse
  }

  setInverse <- function(i) {
    inverse <<- i
  }
  
  list(getMatrix = getMatrix,
       setMatrix = setMatrix,
       getInverse = getInverse,
       setInverse = setInverse)
}

## Given a cacheMatrix, return the inverse. Use the cached value if it exists,
## or cache the newly calculated matrix if needed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (is.null(x$getInverse())) {
    x$setInverse(solve(x$getMatrix()))
  }
  x$getInverse()
}
