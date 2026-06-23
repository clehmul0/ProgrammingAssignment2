## These functions calculate the inverse of a provided matrix and cache it,
## allowing faster retrieval of the inverted matrix. 

## This first function creates a list of functions for later use, storing
## a provided matrix and allowing the user to retrieve the original matrix
## or its inverse (if already cached). 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This second function takes a provided matrix, checks to see if the inverse
## already exists in the cache, and then either returns the cached inverse
## or solves for the inverse and outputs it.

cacheSolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
