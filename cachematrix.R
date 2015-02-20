## These functions can store a matrix and its inverse in cache
## in order to save calucation time when it needs to be used more 
## than once.


## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse of the matrix
## 4. get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) n <<- inverse
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}



## cacheSolve calculates the inverse of the special "matrix" 
## created with makeCacheMatrix(). However, it first checks to see if 
## the invere has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the values of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data)
        x$setinverse(n)
        n  
}
