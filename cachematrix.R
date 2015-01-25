## The functions below will create a list of funcitons for accessing a matrix 
## and calculating and caching the value of its inverse

## Return list of functions for accessing a matrix and its inverse based on a matrix passed into the function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                       ## Stores the inverted matrix
  set  <- function(y){                            ## Sets the value of the matrix and sets the inverted matrix to NULL
    x <<- y
    i <<- NULL
  }
  get <- function() x   ## Gets the matrix
  setinverse <- function(inverse) i <<- inverse   ## Sets the value of the inverted matrix
  getinverse <- function() i                      ## Gets the value of the inverted matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## List of functions returned
}


## Returns the inverse of a matrix
## Value passed in is an R object created via makeCacheMatrix()
## First, it checks to see if the inverse has already been calculated:
## If it has, then the value (the inverse matrix) is retrieved from the cache and returned.
## If not, then the inverse of the matrix passed into the method is calculated, cached and returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()                            ## Get the value (if set) of the inverted matrix
  if(!is.null(i)){                               ## If value is populated, then return cached inverted matrix and exit function
    message("getting cached data")
    return(i)
  }
  ## The below only executes if the inverted value was not found (not cached)
  data <- x$get()                                ## Set data to the value of the matrix passed into function
  i <- solve(data,...)                           ## Calculate the inverted value of the matrix
  x$setinverse(i)                                ## Set value of inverted matrix in the passed R object
  i                                              ## Return inverted matrix
}
