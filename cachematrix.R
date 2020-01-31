## Caching the inverse of a matrix.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Return a CacheMatrix object.
  inverse <- NULL
  get_data <- function() x
  set_data <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(get_data=get_data, get_inverse=get_inverse, set_data=set_data, 
       set_inverse=set_inverse)
}


## Write a short comment describing this function
## This function returns the inverse of a given CacheMatrix object
## if it is cached, or computes the inverse otherwise. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  message("computing inverse")
  inverse <- solve(x$get_data())
  x$set_inverse(inverse)
  inverse
}

## testing: creating a CacheMatrix object
cm = makeCacheMatrix(matrix(rnorm(16), 4, 4))
## computing the inverse 
cacheSolve(cm)
## getting the cached inverse
cacheSolve(cm)
