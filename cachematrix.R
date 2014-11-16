## This functions implements Matrix with built-in caching of the inverse
## Note that the calculation of the inverse sits in the second function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## The following are the function definitions for set, get, setinverse etc.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        ## The return value is a list of the function names
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve calculates the inverse using solve() or returns the cached version
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if already calculated
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if not already calculated, do the actual job
        inv <- solve(x$get(), ...)
        ## cache the value
        x$setinverse(inv)
        ## return the inverted matrix
        inv
}
