## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL #ensures that inv is returned to NULL if the matrix is changed
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated, the
## cachesolve retrievea the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv) # returns the inverse if it was calculated before
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
                inv     #prints the inverse in case it's not calculated before
}
