## The first function makeCacheMatrix creates a list containing a function to
## 1.set the value of the matrix; 2.get the value of the matrix; 
## 3.set the value of the inverse; 4.get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of the matrix created with the above
## function. It first checks whether the inverse has already been calculated.
## If so, it gets the inverse from the cache directly. Otherwise, it calculates
## the inverse of the matrix and sets the value in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
