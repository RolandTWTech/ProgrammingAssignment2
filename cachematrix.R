## Create a pair of functions that cache the 
## inverse of a matrix.

## Creates a special "matrix" object that can
## cache it's inverse.

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


## Computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.  If inverse
## already exists then retreive from cache.


cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ## x %*% x^-1 = identity matrix
     ## x %*% solve(x) = diag(n)
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
