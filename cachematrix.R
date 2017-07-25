## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x  #x is returned 
        setinverse <- function(inversematrix) invmatrix <<- inversematrix #Setter
        getinverse <- function() invmatrix #invmatrix is returned
        list(set = set, get = get, #name each the list elements 
             setinverse = setinverse,
             getinverse = getinverse)
        }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)) {
                message("getting matrix cached data")
                return(invmatrix)
        }
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinverse(invmatrix)
        invmatrix
}

