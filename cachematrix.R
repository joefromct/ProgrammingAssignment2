## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.
## 
## Computing the inverse of a square matrix can be done with the `solve`
## function in R. For example, if `X` is a square invertible matrix, then
## `solve(X)` returns its inverse.

## `makeCacheMatrix`: This function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    # Create/return list 
    list     (set      = set       , 
              get      = get       , 
              setsolve = setsolve  , 
              getsolve = getsolve)
}

##  `cacheSolve`: This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m    <- solve(data, ...)
    x$setsolve(m)
    m
}

# Testing: 
p <- matrix(1:1,1)  
#
t <- makeCacheMatrix(p)
#
cacheSolve(t)
#      [,1]
# [1,]    1
#
# Again, and cached data returned 
cacheSolve(t)
# getting cached data
#      [,1]
# [1,]    1
# 
