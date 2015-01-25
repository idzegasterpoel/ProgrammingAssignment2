## These functions are adaptations to the 'mean' examples from the R Programming Course, 
## programming assignment 2. They can be used to invert a (square) matrix, for which an 
## inverse exists. Note that there is no error trapping implemented. 

## makeCacheMatrix

## makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

## The user should first call this function before attempting to call cacheSolve

makeCacheMatrix <- function(A = matrix()) {
## This function creates a special "matrix" object that can cache its inverse.

    m <- NULL
    set <- function(B) {
        A <<- B
        m <<- NULL
    }
    get <- function() A
	## calculate the actual inverse, using the built-in solve function
    setinverse <- function(solve) m <<- solve 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(A, ...) {
## Return a matrix that is the inverse of 'A'
    m <- A$getinverse()
    if(!is.null(m)) {
        message("getting cached data") ## debugging purposes
        return(m)
    }
    data <- A$get()
    m <- solve(data, ...)
    A$setinverse(m)
    m		
}
