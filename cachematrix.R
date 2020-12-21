## Coursera
## Programming in R
## Week 3 - Programming Assignment
## Laura Johnson

## This file contains code for two functions: makeCacheMatrix() and cacheSolve(),
## * which can be used to compute, store, and retrieve the inverse of a matrix

## The makeCacheMatrix() function computes the inverse of a matrix and 
## * caches it so it can be retrieved without having to recalculate it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set value of the matrix
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the value of the inverse
    setinverse <- function(solve) inv <<- solve
    
    # Get the value of the inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() uses the special matrix object created by makeCacheMatrix
## * and computes the inverse of the matrix if it has not been calculated, or
## * retrieves the cached matrix if previously calculated.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    # If we have already calculated the inverse, retrieves it w/ message
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Otherwise, the inverse is calculated and then retrieved
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    
}
