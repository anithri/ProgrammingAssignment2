## Put comments here that give an overall description of what your
## functions do

## Creates an object that will calculate and cache the inverse of a matrix x

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This method returns a cached or freshly computed inverse of the matrix x
## cacheSolve uses the makeCacheMatrix to

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

