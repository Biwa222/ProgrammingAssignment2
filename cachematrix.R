## This pair of functions allows the user to compute the inverse
## of a matrix while recognizing and providing a cached result for
## repeated requests.  

## Function makeCacheMatrix creates a special matrix object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve receives an input matrix and returns
## the inverse.  If the calculation has been performed to the
## same matrix prior, the cached result is returned.  Otherwise,
## the inverse is calculated using the solve() function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
