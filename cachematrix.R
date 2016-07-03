## Functions below will first create a special matrix that can cache its inverse, then compute the inverse. 
## If the inverse has already been computed, the cacheSolve will retrieve from cache.

## makeCacheMatrix creates special "matrix" object that can cache its inverse, then a list of functions to
## set matrix / get matrix / set inverse / get inverse. The list is used as the input for cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  setMatrix = function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix = function() x
  setInverse = function(inverse) inv <<- inverse 
  getInverse = function() inv
  list(set=setMatrix, get=getMatrix, setinv=setInverse, getinv=getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  ## if the inverse has already been calculated, get cached inverse
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## calculate the inverse 
  temp = x$get()
  inv = solve(temp, ...)
  
  ## set matrix inverse value in the cache using setinv function
  x$setinv(inv)
  
  ## return inverse
  return(inv)
}
