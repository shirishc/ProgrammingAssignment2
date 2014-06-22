## These functions work together to return the cached inverse of a matrix
## if it has already been calculated once. 
## 

## The function makeCacheMatrix helps set up a wrapper over the matrix object 
## to store and retrieve data of a matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    setData <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    getData <- function() x
    setInv <- function(aInv) mInv <<- aInv
    getInv <- function() mInv
    list(setData = setData, getData = getData, setInv = setInv, getInv = getInv)
}


## This is the caller function which will actually be called by the 

cacheSolve <- function(x, ...) {
    mInv <- x$getInv()
    if(!is.null(mInv)) {
        message ("getting cached inverse")
        return(mInv)
    }
    data <- x$getData()
    mInv <- solve(data, ...)
    x$setInv(mInv)
    mInv
        ## Return a matrix that is the inverse of 'x'
}
