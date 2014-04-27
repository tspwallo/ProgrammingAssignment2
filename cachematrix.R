## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(m = matrix()) {
    # Create a special "matrix" which can be used with the cacheSolve() function
    # What it really does is to create a list with the following functions:
    # 1) set the value of the matrix - set()
    # 2) get the value of the matrix - get()
    # 3) set a cached value          - setvalue()
    # 4) get the cahced value        - getvalue()
    # Args:
    #   x: A matrix to convert to a special "matrix"
    # Returns:
    #   A special "matrix", a list of functions
    c <- NULL
    set <- function(y) {
        m <<- y
        c <<- NULL
    }
    get <- function() m
    setcache <- function(value) c <<- value
    getcache <- function() c
    list(set = set, get = get, setcache = setcache, getcache = getcache)
}

cacheSolve <- function(x, ...) {
    # A cached version of the solve() function
    # The idea is that after having computed the inverse we cache the value 
    # and use the cached value in future lookups. 
    # Args:
    #   x: A special "matrix" as a result from cacheMakeMatrix()
    # Returns:
    #   A matrix that is the inverse of 'x'
    m <- x$getcache()    # We have it cached?
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    m <- solve(x$get())  # Solve the inverse
    x$setcache(m)        # And cache the value
    return(m)
}
