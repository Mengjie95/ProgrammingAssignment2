## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    
    # set the matrix
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    
    # get the matrix
    get <- function() {
        m
    }
    
    # set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    # return a list 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Return the inverse if its already set
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data)  %*% data
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}