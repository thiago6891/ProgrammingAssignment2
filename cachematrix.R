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
    ## The inverse is initially set to NULL.
    i <- NULL
    
    ## The set function stores a new matrix "m" and sets the inverse to NULL.
    set <- function(m) {
        x <<- m
        i <<- NULL
    }
    
    ## The get function just returns the currently stored matrix.
    get <- function() x
    
    ## The setinverse function stores the inverse valued passed in.
    setinverse <- function(inverse) i <<- inverse
    
    ## The getinverse function returns the value stored in the variable i.
    getinverse <- function() i
    
    ## A list containing all the 4 previous functions is returned.
    list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function returns the inverse matrix of x, x being the object
## created by the makeCacheMatrix function. If the value is already computed,
## it's immediately returned, otherwise it's calculated and cached for future
## retrieval.
cacheSolve <- function(x, ...) {
    ## Retrieves the stored valued of the inverse and return it
    ## if it's not NULL.
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(m)
    }
    
    ## If the retrieved value is NULL, the inverse is calculated by calling the
    ## solve function and the result is stored in x for future reference.
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    
    ## Finally, the calculated value of the inverse is returned.
    i
}