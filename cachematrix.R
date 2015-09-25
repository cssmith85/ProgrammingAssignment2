## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## Based on the example as a template:
## 1. set the matrix value
## 2. get the matrix value
## 3. set the inverse value
## 4. get the inverse value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set=set,get=get,
       seti = seti,
       geti = geti)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, and ff the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$get()
  if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}