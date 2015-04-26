makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x   <<- y
        inv <<- NULL
    }
    get        <- function()        x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function()        inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Get cached data if there is any, then solve the matrix.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
