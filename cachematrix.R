## This functions will speed up repeatedly computation of a matrix inversion by
## caching the result of the first computation. All following function calls
## will return the cached result.


## makeCacheMatrix: this function function allows to set, get the matrix,
##                  setinv and getinv for the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize cache variable if it is not set
        inv_matrix <- NULL
  
        ## bind functions
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv_matrix <<- inverse
        
        getinv <- function() inv_matrix
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: this function will caclulate the Inverse of a matrix,
##             will return a cached result if it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getinv()
        
        ## check if cached result is available
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        
        ## if no cached result not available, calculate the inverse, cache the result,
        ## return result
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$setinv(inv_matrix)
        inv_matrix
}

## Test sample run commands
## x = matrix(c(1,0.5,0.5,1),nrow = 2,ncol = 2)
## m = makeCacheMatrix(x)
## m$get()
## cacheSolve(m)
## cacheSolve(m)
