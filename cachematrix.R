## Caching the inverse of a Matrix function
##
## Example on how to use it:
##
## > source('cachematrix.R')
## > mCM <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(mCM)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Creating a special "matrix", which is a list containing function to
## set/get the value/inverse value of matrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
