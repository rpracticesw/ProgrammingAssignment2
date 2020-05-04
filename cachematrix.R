## These functions create a matrix that can cache its inverse value, 
## which can then be retrieved from the cache if it has already been calculated. 
## If not, the inverse value is calculated and returned. 

## Creates a special matrix element that can cache its inverse value. 
## A list object is made using the input matrix with the following functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinverse = function(inverse) inv <<- inverse 
    getinverse = function() inv
    list(set=set, 
         get=get, 
         setinv=setinv, 
         getinv=getinv)
}


## This function calculates the inverse of the matrix defined in the makeCacheMatrix function.
## If it has already been calculated, it is retrieved from the cache and the calculation does not occur.
## Then the value of the inverse in the cache is set. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv = x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
    return(inv)
    }
    matrix.data <- x$get()
    inv <- solve(matrix.data, ...)
    x$setinverse(inv)
    inv
}
