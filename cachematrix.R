## Instead of computing the invserse of a matrix repeatedly, we can reduce the run time of our calculation by caching a pre-calculated, unchanged solution.

## This function will create a matrix objext that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<- function(y){
        x<<-y
        inv<<-NULL
}
get<-function()x
setinv<<- function(inverse) inv<<- inverse
getinv<- function()inv
list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## This function will compute the inverse of the matrix created through the function above.
## If the inverse has already been calculated and the matrix has not changed, it will retrieve the inverse from the cache directly.


cacheSolve <- function(x, ...) {
        inv<- x$getinv()
        if(!is.null(inv)){
            message("Getting cached data")
            return(inv)
        }
        matrix<- x$get()
        inv<- solve(matrix,...)
        x$setinv(inv)
        return(inv)
}
