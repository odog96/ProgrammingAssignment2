## Set of functions to invert an input matrix. Checks memory first
 
## makeCacheMatix- 4 functions-1.sets matrix . 2. gets matrix 
## 3. sets inverse 4. gets inverse. Last - all funcitons go in list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
## CacheSolve - 1. checks if inverse is in memory.if so returns from cache, 
#  if so, will get from cache, if not will solve and place into cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
