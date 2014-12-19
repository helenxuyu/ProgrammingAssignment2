## Matrix inversion is usually a costly computation and there may be some
## benefit to chaching the inverse of a matrix rather than compute it
## repeatedly. The following two function could realize the function of
## caching the inverse of a matrix.


## This function creates a matrix, which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function calculates the inverse of the matrix created with makeCacheMatrix.
## It first checks to see if the matrix has already been calculated.
## If so, it get the inverse and skips the calculation.
## Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setInverse function


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m = x$getInverse()
    if (!is.null(m)){
        message("getting cached data")
        return (m)
    }
    data = x$get()
    m = solve(data, ...)
    x$setInverse(m)
    m
}
