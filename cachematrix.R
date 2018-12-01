## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
	i <- NULL
    
    ## set the value of the matrix
    set <- function(y) 
        {
        x <<- y
        i <<- NULL
        }
    
    ## get the value of the matrix
    get <- function()
        x
    
    ## set the value of the inverse matrix
    setinv <- function(solve)
        i <<- solve
    
    ## get the value of the inverse matrix
    getinv <- function()
        i
    
    ## assign functions into the list
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
		 
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## get data from getinv
    i <- x$getinv()
    
    ## if not NULL then return cached data
    if(!is.null(i)) 
        {
        message("getting cached data")
        return(i)
        }
    
    ## if NULL then calculate from the data
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i

}
