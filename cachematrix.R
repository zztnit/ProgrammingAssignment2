## Caching the Inverse of a Matrix
## There are two functions:makeCacheMatrix() and cacheSolve().
## The first one can be used to construct a special matrix object.
## The later one can be used to figure out the inverse  of 
## this special matrix object. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it will use solve() to calculate the inverse.


## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(y) {
        x <<- y
        m_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function( inversem ) m_inverse <<- inversem 
    getinverse <- function( ) m_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_inverse <- x$getinverse()
    if(!is.null(m_inverse)) {
    #message("getting cached data")
      return(m_inverse)
    }
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$setinverse(m_inverse)
    m_inverse
}
