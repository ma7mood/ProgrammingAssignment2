## The two functions are used to create a 'special' matrix object
## that stores a matrix and caches its inverse.

## creates a special matrix object, essentially an 'interface'(list of 4 functions), 
## to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # initialise the inverse to NULL
    inverseOfx <- NULL
    
    # set the value of m to the new value and thus reset inverseOfm to NULL
    set <- function(newValue) {
        x <<- newValue
        inverseOfx <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverseOfx <<- inv
    getInverse <- function() inverseOfx
    
    #returns the list of the 4 functions, our 'special' matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## produces the inverse of the 'special' matrix
## by retrieving it from the cache if present otherwise calculating it
## and setting the inverse in the cache via setInverse function
## the '...' are arguments (if any) to be passed to the internal 'solve' function

cacheSolve <- function(x, ...) {
    
    inverseOfmatrix <- x$getInverse()

    # if inverse is cached then return its value
    if (!is.null(inverseOfmatrix)) {
        message("getting cached data")
        return(inverseOfmatrix)
    }
    
    # if inverse not cached then:
    # 1. calculate it
    matrixData <- x$get()
    inverseOfmatrix <- solve(matrixData, ...)
    # 2. attach it to the matrix
    x$setInverse(inverseOfmatrix)
    # 3. return its value
    inverseOfmatrix
}
