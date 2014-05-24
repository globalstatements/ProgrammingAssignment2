## Matrix object with cache of inverse

## Create a matrix list including a cache of the matrix inverse
## This assumes that only invertible matrices will be
## provided as values

makeCacheMatrix <- function(x = matrix()) {
    ## i is matrix inverse, if available - initialized to null
    i <- NULL
    
    ## set value of matrix
    set <- function(y) { 
    	## x is matrix value
    	x <<- y
    	## inverse is not known yet
    	i <<- NULL
    	}
    ## obtain matrix value x
    get <- function() { x }
    ## invert matrix
    setsolve <- function(solve) { i <<- solve }
    ## obtain matrix inverse
    getsolve <- function() { solve }
    
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Returns the cached matrix inverse
## Or computes if necessary

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
       i <- x$getsolve()
       if (is.null(i) {
       	    ## obtain matrix
       	    data <- x$get()
       	    ## compute inverse
       	    i <- solve(x)
       	    ## cache inverse
       	    x$setsolve(i)
       }
        i
}
