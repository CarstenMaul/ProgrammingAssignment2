## This functions will speed up repeatedly computation of a matrix inversion by
## caching the result of the first computation. All following function calls
## will return the cached result.


## makeCacheMatrix: function allows to set, get the matrix, setinv and getinv for the
## inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize variables
        s <- NULL
        
        ## bind functions
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## cacheSolve: will caclulate the Inverse of a matrix, will return a cached result if
## it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        ## check if cached result is available
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## if no cached result not available, calculate the inverse, cache the result,
        ## return result
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
