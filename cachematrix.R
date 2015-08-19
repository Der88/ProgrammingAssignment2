## The pair of functions makeCacheMatrix and cacheSolve allow the user
## to cache the inverse of a matrix. 

## makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse. It takes an invertible matrix, x, as its argument:

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        ##Set the value of the matrix
        set <- function(y) {
                x<<-y
                im<<-NULL
        }
        ##get the value of the matrix
        get <- function() x
        ##set the value of the inverse matrix
        setim <- function(solve) im <<- solve
        ##get the value of the inverse matrix
        getim <- function() im
        list(set = set,get = get,
             setim = setim,
             getim = getim)
}

## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        im <- x$getim()
        ## Return the cached inverse matrix, if available 
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        ## Calculate and cache the inverse matrix, if unavailable
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
        im
}
