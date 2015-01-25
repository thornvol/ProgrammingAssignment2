## Put comments here that give an overall description of what your
## functions do

## Return list of functions for accessing a matrix and its inverse based on a matrix passed into the function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of a matrix
## Value passed in is an R object created via makeCacheMatrix()
## First, it checks to see if the inverse has already been calculated:
## If it has, then the value (the inverse matrix) is retrieved from the cache and returned.
## If not, then the inverse of the matrix passed into the method is calculated, cached and returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()make
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
