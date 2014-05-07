## The following two functions are used to compute and cache the value of
## the inverse of a matrix.


## The makeCacheMatrix function gets a matrix as input and output a list of
## following functions:
##
## 1) set(m): Sets the data of the current matrix to be the one passed in by
##            the argument m.
##
## 2) get(): Retrieves the current matrix data.
##
## 3) setinverse(i): Sets the value of the inverse matrix to be the one passed
##                   in by the argument i. This function does not calculate the
##                   inverse matrix, it just stores the value passed.
##
## 4) getinverse(): Retrieves the stored value of the inverse matrix, if no
##                  value is store, NULL is returned.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(m) {
        x <<- m
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function returns the inverse matrix of x, x being the object
## created by the makeCacheMatrix function. If the value is already computed,
## it's immediately returned, otherwise it's calculated and cached for future
## retrieval.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}