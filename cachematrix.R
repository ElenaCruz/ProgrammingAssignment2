## This file contains a set of fucntions in order to cache the inverse of a matrix, as 
## the inverse calculation is a costly operation from the computational point of view.
## Two functions are provided:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse. Takes a matrix argument and caches it.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve retrieves the inverse from the cache. Takes 
## the matrix as an argument.


## makeCacheMatrix gets a matrix (x) and creates a special "matrix" object that can 
## cache its inverse. The function provices 4 "methods", by creating a special vector
## with a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse 
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##Inverse initialization
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function returns a matrix that is the inverse of the x matrix argument. 
## It checks whether the inverse for that  matrix is already cached or not. If the 
## inverse has not been cached yet, it calls makeCache matrix to calculate it and build
## a special vector storing it for future calls.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
