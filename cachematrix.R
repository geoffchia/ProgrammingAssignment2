## The following two functions cache potentially time-consuming matrix inversion.
## It makes use of the <<- operator to cache the inverse of a matrix.
## The two functions are used to create a special object that stores a matrix and cache's its inverse.

## This function creates a special "matrix", which is really a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This functions calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data using solve and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
