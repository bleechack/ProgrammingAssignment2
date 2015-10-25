## These functions permit the user to cache computationally intensive operations,
## Specifically matrix inversion

## Create data structure to store the matrix and bind functions to it

makeCacheMatrix <- function(x = matrix()) {
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- solve(x)
    getinverse <- function() i
    list(set = set, get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}


## Lazy load the inverse if possible, calculate it if not

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}


# test the matrix testing tool

sampleMatrix <- matrix(
    c(2, 4, 3, 1, 5, 7, 2, 1, 5),
    nrow=3,
    ncol=3)

x <- makeCacheMatrix(sampleMatrix)
x$set(sampleMatrix)

message("solving the first time (no cache)...")
print(cacheSolve(x))

message("solving the second time (using the cache)...")
print(cacheSolve(x))




