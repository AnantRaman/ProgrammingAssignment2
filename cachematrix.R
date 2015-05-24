#############################################################################################################
##
##                              From Anant Raman romanant77@gmail.com
##
## makeCacheMatrix is a function that creates a special "vector", which is a list of the following methods:
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix
##
## The inverse is stored in the containing environment which gives the impression of the cache
##
############################################################################################################
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}
##
#############################################################################################################
##
## cacheSolve is a function that gets the cached value of the matrix "x".  The inverse is stored in and 
## retrieved from the containing enviroment
##
############################################################################################################
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## If the Cache is not NULL return the cached values with the message and return
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # If it's NULL, it will compute the inverse and save it in the cache and return
        data <- x$get()
        # Invoking the matrix inversion function could use ginv() from 'MASS' package
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
