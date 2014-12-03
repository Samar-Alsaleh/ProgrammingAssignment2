## Samar Alsaleh 
## Dec 3, 2014

## The goal of this code is to cash the inverse of a matrix as it is a costly computation
## and use the casehd value when it's needed

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## Initialize the inverse property
  
  set <- function(y){  ## Method to set the matrix
    x <<- y
    m <<- NULL
  }
  
  get <- function(){  ## Method the get the matrix
    x
  }
  
  setInverse <- function (matrix){ ## Method to set the inverse of the matrix
    m <<- matrix
  } 
  
  getInverse <- function() { ## Method to get the inverse of the matrix
    m
  } 
  
  ## Return a list of the methods
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## check if inverse already exist in cache
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(x)
  ## Return the matrix
  m
}
