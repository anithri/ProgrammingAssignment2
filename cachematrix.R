## Put comments here that give an overall description of what your
## functions do

## Creates an object that will calculate and cache the inverse of a matrix x

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    # Set a new matrix and reset the i to null as it hasn't been computed yet
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # get the matrix
    get <- function() x
    # set the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    # get the inverse of the matrix
    getinverse <- function() i
    #return a list with the 4 functions
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This method returns a cached or freshly computed inverse of the matrix x
## cacheSolve uses the makeCacheMatrix to

cacheSolve <- function(x, ...) {
    #get current inverse
    m <- x$getinverse()
    #if not null we are done
    if(!is.null(m)) {
        message("getting cached data")
        #return the cached inverse
        return(m)
    }
    #otherwise we have to calculate it
    #assign data the matrix
    data <- x$get()
    #create the inverse
    m <- solve(data, ...)
    #store the inverse for next time
    x$setinverse(m)
    #return the calculated inverse
    m
}

