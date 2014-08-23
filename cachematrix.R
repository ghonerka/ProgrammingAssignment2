## The functions makeCacheMatrix() and cacheSolve() can be used to compute and
## and cache the inverse of a matrix.  The cached value can subsequently be 
## retrieved instead of recomputing the inverse.  This may be handy when the 
## inverse of a matrix must be computed repeatedly.


## makeCacheMatrix creates a list of four functions that can set or retrieve the
## value of the matrix (get() and set() functions), and set or retrieve the
## value of the inverse (setInverse() and getInverse() functions).  The set-type
## functions use <<- assignment in order to set values in the parent 
## environment.

makeCacheMatrix <- function(X = matrix()) {
        I <- NULL
        set <- function(Y) {
                X <<- Y
                I <<- NULL
        }
        get <- function() X
        setInverse <- function(inverse) I <<- inverse
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve() uses the functions created by a call to makeCacheMatrix() to 
## compute and cache the inverse of the matrix if it has not yet been cached. 
## If it has already been cached, the cached value is returned rather than
## recomputing the inverse.

cacheSolve <- function(X, ...) {
        I <- X$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- X$get()
        I <- solve(data, ...)
        X$setInverse(I)
        I
}
