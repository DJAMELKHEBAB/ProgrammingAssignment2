## These two function create an object containing a matrix with its inverse.
## the goal is to avoid the calculation of the inverse repeatedly by getting it from the cache

## Build an object that contains a matrix with its inverse and functions to manipulate them

makeCacheMatrix <- function(x = matrix()) {
  ## invx : the inverse of x
  invx <- NULL
  
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  
  get <- function() x
  
  ## 
  setSolve <- function(msolve) invx <<- msolve
  getSolve <- function() invx
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## it calculates the inverse of the matrix on the object created by makeCacheMatrix and store it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getSolve()
  
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  mat <- x$get()
  invx <- solve(mat, ...)
  x$setSolve(invx)
  invx
}
