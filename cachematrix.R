## This assignment is designed to demonstrate the use of environments (<<-)
## By use of the symbol, it causes a new environment to be created.
## The previous "copy" of the matrix is stored in that environment and accessed
## via the cacheSolve function indirectly via embeded functions.

##
## makeCacheMatrix contains the functions for storng the matrix and functions to set and retrieve the matrix.
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

## cacheSolve will create the inverse matrix from the matrix given
## This function assumes if the matrix 
## is found not found in an environment then it gets cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
