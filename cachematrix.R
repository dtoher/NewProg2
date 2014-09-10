## These two functions 'makeCacheMatrix' and 'cacheSolve' work together
## to invert a matrix and to store the result of that inversion in cache.
## An example of how to implement the codes at the end of this code (as a comment)
## These functions make the assumption that the matrix is invertible.
## If the matrix is not square, or is not invertible (singular), 
## error messages will be printed to the screen (from the 'solve' function).

## The function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache its inverse. 
## set and setinverse work on the first run through of the function 
## to store the inverse in cache.
## get and getinverse work to retrieve the inverse from cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function first checks to see if the results of the 
## matrix inversion are already held in cache.

## If the results are not already within the cache it then 
## uses the 'solve' function to invert the matrix
## and stores this result so that it can be called when needed.


cacheSolve <- function(x, ...) {
m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}


## note that the functions need to be run in the following manner;
## Example (not run) to invert a matrix X:
## p<-makeCacheMatrix(X)
## Xinv<-cacheSolve(p)

