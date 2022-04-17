## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. In this assignment we write a pair of 
## functions that cache the inverse of a matrix.

## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
  inverse = NULL
  
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  
  get <- function() matrix
  
  setinverse <- function( inv ) inverse <<- inv
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: this function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated then 
## cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function( makeCacheMatrix, ...) {
  inv <- makeCacheMatrix$getinverse()
  
  if(!is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }else{
    message("NOT getting cached data")
  }
  
  inv <- solve(makeCacheMatrix$get(), ...)
  makeCacheMatrix$setinverse(inv)
  inv
}
