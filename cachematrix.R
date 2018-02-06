# Coursera - R Programming
# Course 2 Week 3 (Loop Functions and Debugging) Assignment
#------------------------------------------------------------
# Two functions: makeCacheMatrix() and cacheSolve()
# Calculate the inverse of a matrix, or return a previously calculated
# inverse for the same (unchanged) matrix if such exists.

#------------------------------------------------------------

# makeCacheMatrix() creates a special "matrix" object that can cache 
# its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

# cacheSolve() computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.It is assumed that the matrix is inversable.
cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if (!is.null(invrs)) {
    message("getting cached matrix")
    return(invrs)
  } else {
    data <- x$get()
    invrs <- solve(data, ...)
    x$setinverse(invrs)
    invrs
  }
}
