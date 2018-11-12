## These two functions serve to cache potentially time-consuming computations.

## The first function, makeCacheMatrix creates a special "matrix" object,
## which is really a list containing a function to set and get the value
## of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL 
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function, cacheSolve, calculates the inverse of the special "matrix" object.
## First it checks if the inverse is cached. If it is, it uses the get function to retrieve
## the inverse from the cache. Otherwise it computes the inverse and sets its value to the cache.

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