
## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
# Generate a series of functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # setFun = function to set the x matrix as a global object and initialize the object m
    setFun <- function(y) {
        x <<- y
        m <<- NULL
    }
#########################################################
    # get = function to get a local copy of the matrix
    getFun <- function() x
    # setSolve = function to apply the solve function to the passed matrix 
    setSolve <- function(solve) m <<- solve
    # getSolve = function to get store the inverted matrix into a new object
    getSolve <- function() m
    list(setFun = setFun, getFun = getFun,
         setSolve = setSolve,
         getSolve = getSolve)
#########################################################
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
# cacheSolve = apply the getSolve function to data set and store into the s object
            # if s is not empty the function returns it
cacheSolve <- function(x, ...) {
    s <- getSolve(x)
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
# assign to "data" object the output of the getFun function applied to x data set
    data <- x$getFun()
# assign to "s" object the output of the solve function applied to "data"
    s <- solve(data, ...)
# get the inverted matrix
   # x$getSolve(s)
    s
}
