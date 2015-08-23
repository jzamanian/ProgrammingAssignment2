## This file contains a set of functions that make a special 
## object, in the structure of a list of functions, that can
## store a matrix and cache its calculated inverse in a separate
## environment from which it can be retrieved without recalculating. 

## This function makes an object in the form of a list functions  
## that collectively can store a matrix and its calculated inverse 
## in a separate environment.

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


## This function checks whether the inverse of a matrix has
## already been calculated. If it has, it retrieves the cached
## solution. If not, it calculates the inverse of the matrix. 
## In either case, it prints the solution.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        
        inverse <- x$get()
        m <- solve(inverse, ...)
        x$setinverse(m)
        m
        
}