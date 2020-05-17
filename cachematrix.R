## This script is created to calculate and cache inverse of a matrix.
##Towards this endeavour, we create two functions : makeCacheMatrix and CacheSolve

## The function makeCacheMAtrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(invrs) inv <<- invrs
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The function cacheSolve computes the inverse of the special matrix created by makeCacheMatrix function.
## It checks first if the inverse of the matrix has already been calculated. If the inverse has already been 
## calculated and cached, it retrives the inverse directly from cache.
## Otherwise, it cacluates the inverse and sets it in the cache.

cacheSolve <- function(x, ...) {
        
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
