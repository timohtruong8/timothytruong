## Caching the Inverse of a Matrix:
## A Matrix Inverse is a costly computation and there may be some benefits in
## caching the inverse of a matrix rather than compute it repeatedly.
## The pair of functions below are used to create a special object that stores a
## matrix and caches its inverse.

## This function creates a special "matrix" where it can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the invere of the special matrix created 
## by the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
