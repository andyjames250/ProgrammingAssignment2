## The functions below allow the inverse of a matrix (assumed to be invertible) 
## to be calculated. The functions support caching of the inverse matrix.

## The function makeCacheMatrix returns a list of functions which can be used
## to set and get a matrix 'x', and its inverse 'i'.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function cacheSolve returns the inverse of 'x'. If the inverse of 'x' has
## already been computed, the cached solution is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (!is.matrix(x$get())) stop("data must be a matrix")
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
