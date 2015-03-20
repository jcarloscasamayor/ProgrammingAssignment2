## The functions create an objecto which is a list of four functions
## that allow us to cache the inverse of a square matrix
## I follow the exameple of mean of this assignment
## Bellow there is an example of their use
## > x<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## > xx<-makeCacheMatrix(x)
## > cachesolve(xx)

## makeCacheMatrix creates a list of four functions:
##      - set: create the object
##      - get: return the original matrix
##      - setinverse: calculate the inverse and cache it
##      - getinverse: returns the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve uses the function makeCacheMatrix as many times as you need
## if the inverse is not cached, it caculates and cache it
## it the inverse is cached, it returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
