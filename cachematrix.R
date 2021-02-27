## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## Following is written a pair of functions, makeCacheMatrix & cacheSolve, that 
## cache the inverse of a matrix.


## makeCacheMatrix <- this function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mr <- NULL
  set <- function(y){
    x <<- y
    mr <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) mr <<- inverse
  getInverse <- function() mr 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve <- this function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mr <- x$getInverse()
  if(!is.null(mr)){
    message("getting cached data")
    return(mr)
  }
  mat <- x$get()
  mr <- solve(mat,...)
  x$setInverse(mr)
  return(mr)
}