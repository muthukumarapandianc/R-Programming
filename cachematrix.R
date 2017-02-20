## Week 3 - R Programming programming assignment - Muthu Chandrasekaran - 02/19/2017 
## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##-----------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
## Intialize to hold the inverse matrix
    	inver <- NULL
## Define function y
	set <- function(y) {
        x <<- y       
#3 Reset inver to NULL for a new matrix   
        inver <<- NULL 
    }
## Return matrix argument
    get <- function() x  
## Assign value of inverse
    setinverse <- function(inverse) inver <<- inverse
## Get value of inverse
    getinverse <- function() inver
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
##-----------------------------------------------------------------------------------------------
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##-----------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
## Matrix to get inverse of x
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("Fetching Cache Data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinverse(inver)
    inver
}
##-----------------------------------------------------------------------------------------------
