## A pair of functions that combined can create an object wrapping a square
## matrix that can cache its inverse to avoid the expense of this operation
## executing more than once.

## 'matrix_val' is a square matrix to be contained in the cache enabled object.
##
## Return an object that contains the input martix_val with the ability to
## cache its inverse.
##
makeCacheMatrix <- function(matrix_val = matrix()) {
    inverse_val <- NULL
    set <- function(new_matrix) {
        matrix_val <<- new_matrix
        inverse_val <<- NULL
    }
    get <- function() {
        matrix_val
    }
    setInverse <- function(updated) {
        inverse_val <<- updated
    }
    getInverse <- function() {
        inverse_val
    }
    list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## 'x' the cache matrix object created by a call to makeCacheMatrix.
## '...' any optional arguments that should be passed to the call to solve()
##        used to invert the contained matrix.
##
## Return the inverted matrix value, this will be calculated on the first call
## but the result will be stored in the matrix object for later use.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(is.null(m)) {
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
    } else {
        message("getting cached data")
    }
    m
}
