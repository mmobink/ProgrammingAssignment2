## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). 
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## Description of function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        nv <- NULL
        set <- function(y) {
                x <<- y
                nv <<- NULL
        }
        get <- function() x
        setnvrs <- function(inverse) nv <<- inverse
        getnvrs <- function() nv
        list(set = set, get = get,
             setnvrs = setnvrs,
             getnvrs = getnvrs)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## The inverse calculates with the matrix not changed, 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        nv <- x$getnvrs()
        if(!is.null(nv)) {
                message("getting cached data")
                return(nv)
        }
        data <- x$get()
        nv <- solve(data, ...)
        x$setnvrs(nv)
        nv
}
