## Simple calculation, caching mechanism for the Matrix Inversions

## Data Structure for the Matrix and its Inverse. It simply gets a matrix as an 
## parameter and encapsulate it with its inverse and provide setter, getter
## options.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## It gets makeCacheMatrix type data structure as a parameter and returns 
## inversion of the matrix which encaplsutated with it. It returns inverse 
## matrix as is if it's already exists inside the makeCacheMatrix, if not it
## calculates and set it inside makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
