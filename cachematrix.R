### this function creates a cache of a given matrix
makeCacheMatrix <- function(x = matrix()) {

m <- NULL
set <- function(y){
	x <<- y
	m <<- NULL
	}
get <- function() x
setinvmatrix <- function(Inv) m <<- Inv
getinvmatrix <- function() m
list(set=set,get=get,setinvmatrix=setinvmatrix,getinvmatrix=getinvmatrix)


}


### this function returns the inverse of a given matrix X, and return it from cache, if already cached...
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmatrix(m)
        m
}
