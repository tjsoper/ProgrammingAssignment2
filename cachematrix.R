## The following two functions solve for the inverse of an invertable matrix and cache the result.

## makeCahceMatrix takes a matrix as its argument and returns a list of four functions  
## that can be used by the cacheSolve function. The inverse of the matrix is cached as the value of 
## the variable i. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL     ## i is set to be NULL until the inverse is computed for the first time
        set <- function(y) {     ## allows makeCacheMatrix to be reset with a new matrix
                x <<- y
                i <<- NULL
        }
        get <- function() { x }     ## returns the matrix argument
        setinverse <- function(inverse) { i<<- inverse }    ## caches the computed inverse
        getinverse <- function() { i }    ## returns the cached inverse
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)  
}


## cacheSolve checks to see if the value of the makeCacheMatrix variable i is NULL
## If i is NULL, cacheSolve calculates the inverse of the matrix returned by makeCacheMatrix  
## and sets makeCacheMatrix variable i to be that result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}