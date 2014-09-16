## Create a new `Cacheable Matrix` that encapsulates `Matrix`

## Create a cacheable Matrix.
## This function takes a normal Matrix can converts it into a Cacheable Matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
}

## Find the inverse of a `Cacheable Matrix`.
## Takes a `Cacheable Matrix` and calculate the inverse of it and store it in cache.
## Subsequent calls will read from cache instead of re-calculating the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
