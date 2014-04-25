## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix accepts a matrix and returns a list of 4 items. It adds caching
## features to the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(z) inverse <<- z
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



## This function returns the inverse of a non singular matrix. If the inverse is
## computed in a previous iteration, the function retrieves the value from cache, else
## the inverse is computed, stored in the matrix and returns the inverse to the calling
## environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        #print("Received the result from Cache")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
