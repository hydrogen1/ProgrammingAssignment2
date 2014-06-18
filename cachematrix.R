## This file contains two functions which facilitate calculating the inverse of a matrix.
##    'makeCacheMatrix' creates a special matrix that can cache its inverse
##    'cacheSolve' first checks if there is a cached inverse for the given matrix, calculating it and saving 
##                  it in the cache for the next time. 
## 

## makeCacheMatrix returns the matrix it is passed, x, in an object containing
##    functions which can set and retrieve a cached version of the inverse of X. 
## 
##    using the result of this function in conjunction with cacheSolve allows quick 
##    calculation of inverses for matrices. 
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  # Function to update matrix stored in cacheMatrix
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Function to retrieve underlying matrix
  getMatrix <- function() x
  
  # Function to cache solved inverse for future use
  setInverse <- function(val) inverse <<- val
  
  # Function to retrieve cached inverse
  getInverse <- function() 
  {
    inverse
  }
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve expects an enhanced matrix produced by sister function makeCacheFunction. 
##  It checks the state of the passed parameter, x, to see if the inverse has already been calculated, 
##    returning it if so. 
##  If the inverse is not saved, cacheSolve solves it, caches it for future use, then returns. 
cacheSolve <- function(x, ...) {
       
  # check if inverse has been cached...  
  inverse <- x$getInverse()
  if(!is.null(inverse)) 
  {
    message("Using cached inverse")
    return(inverse)
  }

  # ...if not, calculate it
  data <- x$getMatrix()
  inverse <- solve(data, ...)
  x$setInverse(inverse) # save for future use
  inverse    
}
