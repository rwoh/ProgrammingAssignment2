## This is a pair of functions 
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve  
##    retrieves the inverse from the cache.

## Function makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
##
## Usage:
##    makeCacheMatrix(x)
##
## Arguments:
##    stored_matrix           A matrix
##
## Return value: 
##    A list of functions: 
##    get()                   Returns the matrix stored in the object
##    set(new_matrix)         Replaces the matrix stored in the object with new_matrix
##    getinverse()            Returns the cached inverse stored in the object (initially NULL)
##    setinverse(new_inverse)  Replaces the cached inverse stored in the object 

makeCacheMatrix <- function(stored_matrix = matrix()) {
    cached_inverse <- NULL
    set <- function(new_matrix) {
        stored_matrix <<- new_matrix
        cached_inverse <<- NULL
    }
    get <- function() stored_matrix
    setinverse <- function(new_inverse) cached_inverse <<- new_inverse
    getinverse <- function() cached_inverse
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.
##
## Usage: 
## cacheSolve(m, ...)
##
## Arguments:
## m      special "matrix" returned by makeCacheMatrix above
## ...    further arguments to be passed to the solve function
##
## Example:  
##     > my_matrix_object = makeCacheMatrix(matrix(1:4, 2,2))
##     > cacheSolve(my_matrix_object)
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
##     > cacheSolve(my_matrix_object)
##     getting cached data
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## try to trieve the cached inverse from x
  cached_inverse <- x$getinverse()
    
    ## if there was a cached inverse, simply return it
    if(!is.null(cached_inverse)) {
        message("getting cached data")
        return(cached_inverse)
    }
  
    ## else, compute the inverse, cache it in x, and return it.
  
    data <- x$get()
    new_inverse <- solve(data, ...)
    x$setinverse(new_inverse)
    new_inverse
}
