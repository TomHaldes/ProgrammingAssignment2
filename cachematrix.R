##
## This code demonstrates how a calculated matrix can be cached to
## eliminate the need to recalculate it if it doesn't change. Thus 
## if it is needed again it can be looked-up instead of recalculated.
## This can be especially useful in a loop.


## The first function, makeCacheMatrix creates a special "vector"
## which is really a list containing a function to:
  ## - set the value of the vector
  ## - get the value of the vector
  ## - set the value of the mean
  ## - get the value of the mean
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function calculates the matrix inverse of the special "vector" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix data and sets the value of the inverse in the 
## cache via the setinverse function.
##

cacheSolve <- function(x, ...) {

  ## Return a matrix m that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    ## message("getting cached data")
    return(m)
  }

  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  ## message("returning re-calculated data")
  m

}
