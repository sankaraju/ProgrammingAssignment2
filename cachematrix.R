## FUNCTION: makeCacheMAtrix 
## DESCRIPTION: Builds and object of functions, stores and returnsthe inverse from the cache 
makeCacheMatrix <- function(x=matrix()) {
        invMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
                
        }
        
        get <- function() x
        
        setinverse <- function(inverse) invMatrix <<- inverse
        
        getinverse <- function() invMatrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## FUNCTION: cacheSolve
## DESCRIPTION; Return a cached inverse of a matrix if available. If not sumputes and stores in the global environment.
## GOTCHAS #1: NO checks to see if x being passed is a matrix or a result of makeCacheMatrix
## ASSUMPTIONS: Assumes that x is list of function objects that contain get, set, setinverse and getinverse functions
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtrx <- x$getinverse()
        
        if (!is.null(mtrx)) {
                message("getting cached data")
                return (mtrx)
        }
        
        data <- x$get()
        
        mtrx <- solve(data, ...)
        
        x$setinverse(mtrx)
        
        mtrx
}