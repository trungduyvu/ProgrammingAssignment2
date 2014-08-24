## Contains a pair of function that allow the computation of matrix's inverse
## to be cached so repeated computations are fast

## Given a matrix, return a list that contain function to 
## - get: Return the original matrix or the last matrix parameter to set
## - set: Override the stored matrix
## - getInverse: Return a stored inverse of the current matrix
## - setInverse: Store the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(new_matrix) {
    x <<-  new_matrix
    inverse <<- NULL
  }
  get <- function() {
    return(x)
  }
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Given a wrapped matrix as returned by the above makeCacheMatrix
## returns the inverse. The inverse is cache so unless the inverse
## has been cleared, subsequent calls to this function will not 
## recompute the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cached_inverse = x$getInverse()
  if (!is.null(cached_inverse)) {
    message("getting cached data")
    return(cached_inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
