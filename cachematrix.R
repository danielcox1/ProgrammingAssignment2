## These functions allow the user to cache the inverse of a matrix.
## This can save computation time if the inverse is needed many times.

## Create a matrix object that can cache its inverse
makeCacheMatrix <- function(x=matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Compute the inverse of the matrix or return the cached calculation if available
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        #message("getting inverse from cache...")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setInverse(i)
    return(i)
}

# Test code to prove cache is faster than recomputing
# options(max.print=1)
# m <- matrix(data=rnorm(10*10), nrow=10, ncol=10)
# cm <- makeCacheMatrix(m)

# ptm <- proc.time()
# replicate(10000, {solve(m)})
# proc.time() - ptm

# ptm <- proc.time()
# replicate(10000, {cacheSolve(cm)})
# proc.time() - ptm
