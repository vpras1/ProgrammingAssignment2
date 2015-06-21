## The two functions below create an invertible matrix 
## that can be cached and the calculate the inverse for 
## the aforementioned matrix, updating the inverse from 
## the cache when necessary.

## makeCacheMatrix creates a list of functions that allow the
## user to set the matrix, get the matrix and set and get the 
## matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cache solve returns the inverse of the matrix from the
## cache when it hasn't been updated and calculates the 
## new inverse after it has been updated or is null.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
