## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse of the Matrix
## get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {


            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinverted <- function(inverted) inv <<- inverted
            getinverted <- function() inv
            list(set = set, get = get,
                 setinverted = setinverted,
                 getinverted = getinverted)
}





## This function calculates the inverted of the special "Matrix"
## created with the Above function. However, it first checks to see if the
## Inverse Matrix has already been calculated. If so, it `get`s the Inverse Matrix from the
## cache and skips the computation. Otherwise, it calculates the Inverse Matrix of
## the data and sets the value of the Inverse in the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverted()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$inverted(inv)
        inv
}
