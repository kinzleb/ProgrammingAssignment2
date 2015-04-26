## First call makeCacheMatrix passing in a matrix and assign a variable the
## list object that is returned by it
## Then call cacheSolve passing in the variable holding the list object
## The first call will have to calculate the inverse of the matrix but then
## suqsequent calls will pull the inverse matrix from cache

## function stores a list of functions that will get or set a matrix and
## get or set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function uses makeCacheMatrix and operates on the functions of the list
## object from it to either get the cached inverse matrix or calculate, set to cache,
## and return the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    ## Only pull from cache if there is a cached valuen
    if(!is.null(m)){
        message("getting the cached inverse matrix")
        return(m)
    }
    
    data <- x$get()
    
    ## If unable to get the inverse matrix from cache then compute it and cache it
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
