
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        makeCacheMatrix <- function(x = matrix()) {
                minv <- NULL
                set <- function(y) {
                        x <<- y
                        minv <<- NULL
                }
                get <- function() x
                setinv <- function(inv) minv <<- inv
                getinv <- function() minv
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        }
        
        

}



## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x, ...) {
                m <- x$getinv()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data)
                x$setinv(m)
                m
        }
        ## Return a matrix that is the inverse of 'x'
}
