## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function "makeCacheMatrix" creates a matrix object that caches its
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <-function (y) {
                x <<- y
                invs <<- NULL
        }
        get <-function() x
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set=set, get=get,
             setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function "cachesolve" computes the inverse of the matrix returned
## by "makeCacheMatrix". If already calculated "cacheSolve" retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        if(!is.null(invs)) {
                message("Retrieving data from cache")
                return(invs)
                
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinverse(invs)
        invs
}
