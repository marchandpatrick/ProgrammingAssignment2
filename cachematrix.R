## This function creates a special "matrix" object that can cache its inverse.

## X is suposed to be a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    
  m <- NULL
  ## sets the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the matrix
  get <- function() x
  ## calculates the inverse of the matrix
  setinverse <- function(solve) m <<- inverse
  ## get the inverse of the matrix
  getinverse <- function() m
  ##
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, we assume that the matrix supplied is always invertible.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
  ## gets cached matrix if available
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## otherwise, compte it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
