## This is part of Homework Assingment 2 for R Programming in Coursera.

## The purpose of this function pair is to solve for the inverse of a matrix and store it in cache.
## Then to return the inverted matrix whenever it is requested.

## The main point is that inverting a matrix can be costly.

## This function is used to get and set the inverse of the matrix.

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


## This function's purpose is to invert the matrix and return it.

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
