## Coursera R Programming Assignment 2
## By: Rohit Joshi
## 
## This file contains two functions. Function makeCacheMatrix is passed
## an invertible matrix object and returns a list that represents a 
## cacheable matrix. Function cacheSolve determines the inverse of a cached
## matrix either by solving for this value or finding the value from the cache.

## Make CacheMatrix is passed an invertible matrix and returns a special "cache"
## matrix which is a list containing the original matrix value and matrix's 
## inverted value.

  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
}

## cacheSolve takes a "cache" Matrix and returns the inverse of
## the matrix, either by solving for this value directly or returning
## the saved cache value.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}