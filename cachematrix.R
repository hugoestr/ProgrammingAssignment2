## We have two functions that work together
## makeCacheMatrix creates a container that holds a matrix and its inverse, if available
## and cacheSolve is a function that will give the matrix's inverse or retrieve a cached version
## if available

## A function that creates a container that holds a matrix and possibly a cache of its inverse
## it uses R's closure mechanism to hold the values in an environment

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverseInput) inverse <<- inverseInput
  getInverse <- function() inverse
  
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSove will either return a cached inverse or solve it
## The first part of the function will query the matrix container to see if it has 
## a cached result. If it doesn't, then it retrieves the matrix, caculates the inverse 
## using the solve() function, save that value in the cache, and then return the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)

  inverse
}
