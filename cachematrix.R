
## 'makeCacheMatrix' stores a matrix and its inverse. 
## It takes one argument, a matrix x,
## and returns a list of four functions that
## get/set x and get/set the inverse of the matrix x. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## 'cacheSolve' returns the inverse of a matrix x which is given by
## the function 'makeCacheMatrix'. 

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
