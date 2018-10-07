##################################################################
##  Oliver Zarate,  October 10, 2018 
##  Coursera R Programming Week 3, programning assignment 2
##  makecachematrix and cachesolve functions - lexical scoping
##
##################################################################
##  The set of functions below makecachematrix and cacheSolve work 
##  in conjuction to provide the inverse of an input matrix, but
##  the functions will first check to see if given input has already
##  be solved for, and then simply pull from cache instead of
##  calculating. 
#################################################################
## makecachematrix -function created to support CacheSolve function 
## details shown in comments below. Requires input matrix and assignment
## to object. Then can use cachematrix funtion afterwards.
makecachematrix <- function(x = matrix()) {
    ## 1. Resets the "inv.matrix" solution.
    inv.matrix <- NULL
    set <- function(y) { 
        x <<- y             # assigns input matrix to x
        inv.matrix <<- NULL # 1. Resets the "inv.matrix" solution
    }                       # & assigns to parent environment.
    get <- function() x     # function to return input matrix
    ## function to assign solution in parent environment 
    setsolve <- function(solve) inv.matrix <<- solve
    ## funtion to get solution 
    getsolve <- function() inv.matrix
    ## creates a list object of funtion just created
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
## function below is manages calculation method of input matrix 
## Input argement 'x' must be a list created by makecache matrix 
## function

## function first checks if inv.matrix exists, which is why earlier
## funtion assigned to parent argument. If it does exist (i.e. not null)
## then it simply returns from inv.matrix variable
cacheSolve <- function(x, ...) {
    inv.matrix <- x$getsolve()
    if(!is.null(inv.matrix)) {
        message("getting cached data")
        return(inv.matrix)
    }
    ## if inv.matrix is null, then uses get funtion to get input 
    ## matrix & assigns to 'data'. Then uses solve to calculate
    ## inverse matrix, then uses setsolve function to set solution
    data <- x$get()
    inv.matrix <- solve(data, ...)
    x$setsolve(inv.matrix)
    inv.matrix
}
###############################################################
### Test Script
# set seed
set.seed(2) 
# creating a 4x4 matrix from random normal function
amatrix<-matrix(rnorm(1:16),nrow=4)  
# initializing to makeccachematrix fx
mymatrix<- makecachematrix(amatrix)
## run twice. 2nd time check to make sure to see message 
## "getting cached data"
cacheSolve(mymatrix)

