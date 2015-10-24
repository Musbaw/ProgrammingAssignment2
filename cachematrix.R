## Matrix inversion is usually a costly computation and there may be 
## benefit to caching the inverse of a matrix rather than computing
## it repeatedly. The pair of functions below cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the "special" matrix.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips computation.
## else, it calculates the inverse of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
