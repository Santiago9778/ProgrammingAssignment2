# This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(matrix);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(matrix, ...) {
    inverse <- matrix$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data... :)")
        return(inverse)
    }
    data <- matrix$get()
    invserse <- solve(data, ...)
    matrix$setinv(inverse)
    return(inverse)
}
