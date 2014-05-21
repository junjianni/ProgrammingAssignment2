## R Program Assignment 2 
## In the assignments, two functions will written to help cache potentially
## time-consuming computation, calculating the inverse of a matrix.

## The first function, 'makeVector' creates a special "vector", which is 
## really a list containing a function to: 1) set a matrix; 2) get a matrix;
## 3) set the inverse of matrix; 4) get the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        # ginv is able to get the inverse of matrix if it's not square matrix.
        # library(MASS)
        # setsolve <- function(ginv) s <<- ginv
        
        getsolve <- function() s
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The following function calculates the inverse of a matrix. It will first
## checks to see if the inverse of matrix has already been calculated. If so, it `get`s the mean from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and then sets the value of the inverse of matrix in the cache 
## via the `setsolve' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
