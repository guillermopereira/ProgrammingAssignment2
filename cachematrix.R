## Programming Assignment 2
##
## Caching the inverse of a matrix
## Given the computational cost of matrix inversion it can be valubale
## to cache the inverse of a matrix for later use. The following functions
## are developed to cache the inverse of a matrix.
##
##
##
## makeCacheMatrix creates a matrix object that can store its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve outputs the inverse of the matrix given by makeCacheMatrix.
## If the inverse has already been calculated, the funciton retrieves it
## from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Getting cached inverse matrix")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
