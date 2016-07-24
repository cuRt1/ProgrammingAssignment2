# The following two functions can be used to cache the inverse of a matrix.

## makeCacheMatrix
## Creates a special 'matrix' object that can cash its inverse.
## Creates functions to be used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve
## Computes the inverse of the special 'matrix' object created by makeCacheMatrix.
## If the inverse is already cached, this function retreives the inverse
## from the cache rather than computing it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinv(inv)
  inv
}
