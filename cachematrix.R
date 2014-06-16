## R Programming Course, june 2014 Data Science Specialization, Coursera
## 

## Function makeCacheMatrix creates an object of class list, whose elements are functions:
## 'setmatrix': saves a matrix on the cache (creates a matrix element on cache)
## 'getmatrix': gets a saved matrix from the cache
## 'setinvmatrix': saves the inverse of the saved matrix on the cache
## 'getinvmatrix': gets the saved inverse of the saved matrix from the cache
## IMPORTANT NOTE: input matrix at 'setmatrix' must be invertible

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {    # saves a new matrix to cache and resets inverse on cache
        x <<- y
        m <<- NULL
    }
    get <- function() x                 # retrieves matrix from cache
    setinv <- function(inv) m <<- inv   # sets inverse matrix to cache
    getinv <- function() m              # saves inverse matrix to cache
    ## returns a list object with our four functions
    list(setmatrix = set, getmatrix = get,
         setinvmatrix = setinv,
         getinvmatrix = getinv)
}


## Function cacheSolve returns the inverse matrix of the input x, 
## being 'x' an object created by makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix using solve function and 
## sets the value of the inverse in the cache via the setinvmatrix function 

cacheSolve <- function(x, ...) {
    m <- x$getinvmatrix()   # retrieve the inverse matrix from cache
    # if already exists, it's returned directly as output
    if(!is.null(m)) {       
        message("getting cached data")
        return(m)
    }
    # if not, it's calculated, saved to the cache, and returned as output
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setinvmatrix(m)
    m
}
