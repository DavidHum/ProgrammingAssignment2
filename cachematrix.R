## These function are used to make a matrix object that can cache its inverse
## and return the cached inverse of that matrix

## makeCacheMatrix makes a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve then computes the inverse of the special matrix returnet by the
## function makeCacheMatrix.
## If the inverse has alredy been calculated and the matrix is the same
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
