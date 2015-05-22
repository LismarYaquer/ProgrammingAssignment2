## Put comments here that give an overall description of what your
## functions do

## I have to excuse for my english, I hope you can understand the descriptions
## I am from Venezuela and I do not master the language, please consider it :(

## Write a short comment describing this function
## Creates an special structure for matrices, that can't be store in memory
## its a list contaning 4 functions that allows to store and retrieve a matrix
## and its inverse to an object in a different environment
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Returns the inverse of the matrix stored in x, either the one stored in x
## or the one calculated using the 'solve' function
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    if(!is.null(data)) {
        inv <- solve(data, ...)
        x$setinverse(inv)
    }
        ## Return a matrix that is the inverse of 'x'
    inv
}
