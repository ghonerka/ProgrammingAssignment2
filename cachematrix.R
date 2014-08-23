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
        # The inverse value is stored in the variable I
        I <- NULL
        
        # set() sets the value of the matrix
        set <- function(Y) {
                X <<- Y
                I <<- NULL
        }
        
        # get() retrieves the value of the matrix
        get <- function() X
        
        # setInverse() sets the cached value of the inverse
        setInverse <- function(inverse) I <<- inverse
        
        # getInverse retrieves the cached value of the inverse
        getInverse <- function() I
        
        # Return the list of these four functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve() uses the functions created by a call to makeCacheMatrix() to 
## compute and cache the inverse of the matrix if it has not yet been cached. 
## If it has already been cached, the cached value is returned rather than
## recomputing the inverse.

cacheSolve <- function(X, ...) {
        # Retrieve the cached value of the inverse
        I <- X$getInverse()
        
        # If a value has been cached, return it without computing the inverse
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        
        # Otherwise, get the value of the matrix, compute and cache the inverse
        data <- X$get()
        I <- solve(data, ...)
        X$setInverse(I)
        
        # Return the inverse
        I
}
