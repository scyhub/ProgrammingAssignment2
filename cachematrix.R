## The functions makeCacheMatrix and cacheSolve below cache the 
## inverse of a matrix


## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. This special "matrix" is really a
## list containing a function to:
## 1. set the value of the matrix (set())
## 2. get the value of the matrix (get())
## 3. set the value of the inverse of the matrix (setinverse())
## 4. get the value of the inverse of the matrix (getinverse())

makeCacheMatrix <- function(x = matrix()) {
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


## The cacheSolve matrix computes the inverse of the special 
## "matrix" returned by the function makeCacheMatrix above. 
## However, if the inverse has been already calculated (and 
## the matrix has not changed), then the cacheSolve retrieves 
## the inverse from the cache:

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
