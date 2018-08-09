# Programming assignement - Week 3 - R Programming coursera cours

## pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invM<-NULL
        set<-function(y){
                x<<-y
                invM<<- NULL
        }
        get<-function()x
        setinvM<-function(solve)invM<<-solve
        getinvM<-function()invM
        list(set = set, get = get,
             setinvM = setinvM,
             getinvM = getinvM)
        
}


## cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
        invM <- x$getinvM()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setinvM(invM)
        invM ## Return a matrix that is the inverse of 'x'
}

