
## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  z <- NULL
  set <- function(u) {
    m <<- u
    z <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function returns the inverse of the matrix. However first it checks if the inverse has already been computed. If so, it gets the result and skips th computation.
#If not, it computes the inverse, sets the value in the cache via setinverse function.

# This following function assumes that the matrix is always invertible (according the task).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(m, ...) {
    z <- m$getinverse()
    if (!is.null(z)) {
      message("Cached data")
      return(z)
    }
    data <- m$get()
    z <- solve(data, ...)
    m$setinverse(z)
    z
  }
