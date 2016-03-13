## Caching the Inverse of a Matrix:
## Rather than compute it repeatedly, the inverse of a matrix can be catched.
## The benefits are great since matrix inverse computation is a costly task.

## The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## This function computes the inverse of the matrix produced by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        mat <- x$get()
         inv <- solve(mat, ...)
         x$setInverse(inv)
         inv
}

