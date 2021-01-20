## Two functions that will cache 
## the inverse of a matrix
## The two functions are makeCacheMatrix
## and cacheSolve

makeCacheMatrix <- function(x = matrix(1:100,9),3,3)) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
 }
##
## Used to get the cache data
cacheSolve <- function(x, ...) 
        {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data!")
                return(inv)
         }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv   
}
