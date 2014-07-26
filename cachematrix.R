## russoz777
## Coursera Course - R Programming
## Assignment #2 - Caching the Inverse of a Matrix

## This file contains two functions, makeCacheMatrix and 
## cacheSolve.

## Function: makeCacheMatrix()
## The makeCacheMatrix funtion creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Function: cacheSolve()
## The cacheSolve function computes the inverse of the special 
## "matrix" returned by the makeCacheMatrix funtion.If the inverse
## has already been calculated (and the matrix has not changed),
## then this function should retrieve the inverse from the cache.
##
## this function assumes that the matrix supplied is always
## invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'       
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- Solve(data, ...)
    x$setSolve(m)
    solve(x)
    m
    
}
