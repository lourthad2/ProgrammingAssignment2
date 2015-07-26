## Function for cache and compute the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  ##set the value of the matrix
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  ##get the value of matrix
  get <- function() return(mtx);
  ## set the value of inverse matrix
  setinv <- function(inv) inverse <<- inv;
  ##get the value of inverse matrix
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the "matrix" returned by 
## makeCacheMatrix above.It first checks if the inverse has already been 
## computed.If so, it gets the result and skips the computation. If not, 
## it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}