## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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