## Put comments here that give an overall description of what your
## functions do

## makeCacheMatric() creates a matrix of functions that 
## set or get the value of a matrix and
## set or get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    theinverse <- NULL
    set <- function(y) {
        x <<- y
        theinverse <<- NULL
    	}

    get <- function() x
    setinv <- function(solve) theinverse <<- solve
    getinv <- function() theinverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)

    }


## cacheSolve() provides the inverse of a special matrix created by
## the above makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    theinverse <- x$getinv()

    if(!is.null(theinverse)) {
        message("getting cached data")
        return(theinverse)
        }

    data <- x$get()
    theinverse <- solve(data, ...)
    x$setinv(theinverse)
    theinverse 

    }
