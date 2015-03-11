## R Programming (Coursera)
## Programming Assignment 2
## March 11, 2015
## Author: Madison Hale

## This function creates a special "matrix" object that can cache its inverse
## Assumption: the matrix supplied is always invertible
  makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    ## Set the value of the matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() x
    
    ## Set the value of the inverse matrix
    setinverse <- function(inverse) m <<- inverse
    
    ##Get the value of the inverse matrix
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
  }


## This function computes the inverse of the special "matrix" returned by
## makCacheMatrix above.If the inverse has already been calculated 
## (an the matrix has not changed), then the cachesolve should retrieve
## the inverse of the cache.
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  ## If mean has alread been calculated,
  ## get mean from cache and skip computation
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  data <-x$get()
  
  m <- solve(data) %*% data
  
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
