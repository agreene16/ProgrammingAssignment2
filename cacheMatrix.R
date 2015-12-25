#The first function creates a list that contains various functions within the context of a matrix input, and the second function takes this input to solve or simply retrieve#
#an output based on the initial input x#

## makeCacheMatrix takes the input of matrix x and creates a list of functions that will operate within the environment x when
## evaluated by cachesolve
makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <- solve
        getinverse <- function() m 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cachesolve takes the output of makeCacheMatrix, an atomic vector of functions operating within environment x, and thereby either 
## retrieves an inverted matrix that exists as cached data, or solves an invertible matrix that has not yet been solved
cachesolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
