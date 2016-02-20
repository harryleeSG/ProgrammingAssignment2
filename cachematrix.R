## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix creates a special matrix object, 
## while the cacheSolve calculates the inverse of the matrix.
## If the inverse of the matrix has already been calculated, then 
## it is retrieved from the cache and not calculate it.

makeCacheMatrix <- function(x = matrix()) {
  ## Create a special list vector that holds the inverse of a matrix, 
  ## if it has been calculated before
  elc <- NULL
  set <- function(y) {
    x <<- y
    elc <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) elc <<- inverse
  getinverse <- function() elc
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## The function cacheSolve returns the inverse of the matrix
## created with the makeCacheMatrix function.
## If the cached inverse is present, cacheSolve returns the stored 
## inverse of the matrix, otherwise it computes, caches and returns
## the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  elc <- x$getinverse()
  if (!is.null(elc)) {
    message("Getting cached inverse of the matrix.")
    return(elc)
  } else {
    data <- x$get()
    elc <- solve(data)
    x$setinverse(elc)
    return(elc)
  }
}