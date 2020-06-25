## The following pair of functions cache the inverse of a matrix
## The makeCacheMatrix function creates a cached "matrix".
## which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function()x
        setInverse <- function(solve) a <<- inverse
        getInverse <- function() a
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The CacheSolve function computes the inverse of the object
##returned by makeCacheMatri above. If the inverse has already been calculated,
##cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() #
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat,...)
        x$setInverse(m)
        m
}