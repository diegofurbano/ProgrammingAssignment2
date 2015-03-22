## Thisprogram creates 2 functions that compute the inverse of a matrix
## if the inverse was calculated previously the program does not compute
## the inverse, but takes the result from the previous calculation.

## This function calculate the inverse of a matrix and cache it 

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}





## This function return the inverse of a matrix,
## it calculate it just if it wasn't calculated before,
## Otherwise it will take the result from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
