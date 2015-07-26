## This following code defines functions 
## to cache and compute the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. It specifies
## methods to get/set the matrix as well as matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(mtx) {
    x <<- mtx;
    inverse_matrix <<- NULL;
  }
  get <- function() return(x);
  setinverse <- function(inv) inverse_matrix <<- inv;
  getinverse <- function() return(inverse_matrix);
  return(list(set = set, get = get, 
              setinverse = setinverse, getinverse = getinverse))  
}

## This function computes the inverse of the "matrix" object
## returned by makeCacheMatrix. When the matrix inverse has
## already been calculated (and matrix has not changed), 
## cacheSolve retrieves the matrix inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  return(inverse)
}