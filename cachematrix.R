## After inputing a square matrix 'x', makeCacheMatrix and cacheSolve
## return the inverse of the matrix.

## makeCacheMatrix will compute and cache the inverse of
## a square matrix 'x' as a special matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## set matrix value
        inve <- NULL
        set <- function(y) {
            x <<- y
            inve <<- NULL
        }
    ## get matrix value
        get <- function() x
    ## set matrix inverse
        setinverse <- function(inverse) inve <<- inverse
    ## get matrix inverse
         getinverse <- function() inve
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will retrieve from cache or compute the 
## inverse of the special matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    ## get cached matrix inverse  
        inve <- x$getinverse()
    ## check if inverse of 'x' calculated
        if(!is.null(inve)) {
            message("getting cached data")
            return(inve)
        }
    ## or if not calculate       
        data <- x$get()
        inve <- solve(data, ...)
    ## set matrix inverse
        x$setinverse(inve)
    ## return special matrix
        inve
}
