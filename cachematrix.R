## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special matrix, which in fact returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

# This funtions returns the inverse of the matrix 'x'. Firstly, it looks if the inverse is cached. If yes, it just returns it.
# If not cached, it evaluates the inverse, cache it and then return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
