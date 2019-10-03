## Two functions that 
## 1) cache the inverse of a matrix, and
## 2)  recall that cached value when called


## makeCacheMatrix creates a special "matrix object" that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Turns a matrix into a special "matrix"
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve determines whether the inverse of a special "matrix object" 
## (returned by makeCacheMatrix above) has been cached. If the 
## inverse hasbeen cached and the matrix is unchanged, it returns
## the cached value, else it calculates the inverse.
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}