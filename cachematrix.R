## Functions to cache matrix inversion.
##
## The functions in this module provide a convenient interface
## to access and cache the inverse of a matrix. Example usage:
##
## Create an object to store the matrix and cache its inverse
## test <- makeCacheMatrix(matrix(1:4, 2, 2))
##
## # The first call to cacheSolve will calculate the inverse,
## inverse1 <- cacheSolve(test)
##
## # Further calls to cacheSolve will be significantly faster, since
## # they return the value cached during the previous call.
## inverse2 <- cacheSolve(test)


## makeCacheMatrix -- creates an object to hold a matrix and (eventually)
## its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                ## create new special matrix and set its inverse to NULL
                x <<- y
                inverse <<- NULL
        }
        get <- function() x  ## return matrix
        setInverse <- function(solve) inverse <<- solve  ##set inverse value
        getInverse <- function() inverse  ## get inverse value
        ## return list of functions that gets/sets matrix/inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve -- returns the inverse of the matrix stored in a
## 'makeCacheMatrix' structure. Further calls to cacheSolve() for
## the same 'makeCacheMatrix' structure will return a cached inverse.
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse() #
        if(!is.null(inverse)) {
                ##inverse is already cached. Return it
                return(inverse)
        }
        ## We don't have inverse yet. calculate it
        data <- x$get()
        inverse <- solve(data, ...)
        ## cache it
        x$setInverse(inverse)
        ## return it
        inverse
}
