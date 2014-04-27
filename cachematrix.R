## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
### This is a bit dumb cache, as it doesn't take into consideration that '...' arguments that might have changed in the second call
### and returns the first value, even though calling solve with extra arguments would error
### e.g. solve(my_matrix, mean)
### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
### whereas:
### cached = makeCacheMatrix(my_matrix)
### cacheSolve(cached) gives an inverse
### cacheSolve(cached, mean) second call gives the same result even though it should normally error
### The solution would be to either hash all arguments or store them in a list that would be a key for getting cached martices,
### but that is beyond this programming assignment.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
