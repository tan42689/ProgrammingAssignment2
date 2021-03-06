## Put comments here that give an overall description of what your
## functions do

##The first function, makeVector creates a special "vector", which is really a list containing a function to

##set the matrix
##get the matrix
##set the Inverse matrix
##get the Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solve for inverse matrix.
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the Inverse from the cache and skips the computation. 
##Otherwise, it calculates the Inverse of the matrix and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

