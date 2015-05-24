## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.
## Assume that the matrix supplied is always invertible.

# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
        x <<- y
        inverse <<- NULL
      }
      get <- function() x
      setinv <- function(inv) inverse <<- inv
      getinv <- function() inverse
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## get the inverse of matrix in object 'x'
    ## return message and the inverse of matrix in object 'x' if inverse is not null
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse of matrix")
        return(m)
    }
    
    ## use solve() to return the inverse of matrix in object 'x'
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
