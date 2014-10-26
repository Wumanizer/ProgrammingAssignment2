## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # instantiation of "inverse" variable in the environment of makeCacheMatrix()
  set <- function(new_x) {
    x <<- new_x # look for an existing instance of x in the parent environment (which is of makeCacheMatrix) and assign new_x to it
    inverse <<- NULL # since we changed our matrix, we also have to reset the inverse of it
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,    # make setters/getters accessible outside of makeCacheMatrix()
       setinverse = setinverse, # lefthandside is external name, righthandside the internal name
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) { # check if inverse is precomputed, if so take the cached data...
    message("getting cached data")
    return(inverse)
  }
  # ...otherwise we have to calculate the inverse
  data <- x$get() # get matrix
  inverse <- solve(data, ...) # compute inverse of matrix
  x$setinverse(inverse) # cache inverse
  inverse
}