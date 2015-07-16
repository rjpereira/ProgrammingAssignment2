## This pair of functions allow to calculate the inversion of a matrix, 
## caching its result. 

## makeCacheMatrix :creates a special "matrix" object that can cache its 
## inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # Initialize accessors to object
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    
    # Return access methods
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve: Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

    # Try to get cached result
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # We need to calculate it....
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
