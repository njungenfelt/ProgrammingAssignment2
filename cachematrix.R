## Below are two functions that are used to create a special object that stores
## a matrix and caches its inverse.

## This first function creates a list of functions to set/get the matrix or its
## inverse.
## Example: cMat <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solveInv) inv <<- solveInv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the inverse of the matrix from the above
## function. However, it first checks to see if the inverse has already been
## calculated and if so gets the inverse from the cache.
## Example: cacheSolve(cMat)

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}