## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() 
                x
        setinverse <- function(solve) 
                s <<- solve
        getinverse <- function() 
                s
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then the cachesolve retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
        print(s)
        ## Return a matrix that is the inverse of 'x'
}
