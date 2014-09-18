## This is part of Homework Assingment 2 for R Programming in Coursera.

## The purpose of this function pair is to solve for the inverse of a matrix and store it in cache.
## Then to return the inverted matrix whenever it is requested.

## The main point is that inverting a matrix can be costly.
## This function is used to get and set the inverse of the matrix - especially if it is large.

## Functions:
## 1. set = set the value of the matrix
## 2. get = get the value of the matrix
## 3. setinverse = set the inverse of the matrix in cache
## 4. getinverse = get te inverse of the matrix in cache

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


## This function's purpose is to check to see if the matrix was inverted.  If it was - return cached data.
## If it was not inverted, then invert and set cache.

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


## Example of the code running:
## Creating the Cache Matrix:
## > mat <- makeCacheMatrix(matrix(c(1,2,3,4), 2,2))

## Checking to see if the matrix was set:
## > mat$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## Running cacheSolve on the matrix for the first time - matrix wasn't inverted so it will invert and solve:
## > cacheSolve(mat)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Running cacheSolve on the matrix for the second time - matrix was inverted so getting data from cache:
## > cacheSolve(mat)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Showing values set for get() and getinverse()
## > mat$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > mat$getinverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5