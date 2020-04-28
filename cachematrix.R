## Put comments here that give an overall description of what your
## functions do
## Our aim in this experiment is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix). 
## This is useful because the computation takes a lot of memory, and this refers to the results of the computations instead of doing them again and again needlessly, thus saving time and resources.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
}

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) 
    {
        message("Matrix inversion cache")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
