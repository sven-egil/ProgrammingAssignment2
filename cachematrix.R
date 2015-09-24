## The makeCacheMatrix() and cacheSolve() functions calculates and caches the inverse of a matrix 
## If the calculation has been done and the matrix is unchanged, the cached result will be used
## instead of calculating it again.

## makeCacheMatrix() creates a special matrix that sets and gets the value of the matrix and
## then sets and gets the inverse value of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                # set the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                 # get the matrix
  setinverse <- function(solve) m <<- solve   #set the inverse matrix outside the environment
  getinverse <- function() m          # get the inverse of the matrix
  list(set = set, get = get,          # return a list of the sets and gets
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() checks to see if the inverse of the matrix has already been calculated.
## If it has it gets it from the cache and skips the computation, if not it calculates
## the inverse of the matrix and sets the result in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                 # get the inverse matrix
  if(!is.null(m)) {                   # check if it exists in cache
    message("getting cached data")
    return(m)                         # and return it if it does
  }
  data <- x$get()                     # if not, get the matrix
  m <- solve(data, ...)               
  x$setinverse(m)                     # and calculate the inverse
  m
}
