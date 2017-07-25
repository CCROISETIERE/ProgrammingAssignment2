## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL ## reset solve cache variable when matrix is modified
        set <- function(y) { ## setter to pass y to x in the parent environment
                x <<- y
                invmatrix <<- NULL ## reset solve cache variable when matrix is modified
        }
        get <- function() x  ##  getter will return x 
        setinverse <- function(inversematrix) invmatrix <<- inversematrix ## setter in parent environment 
        getinverse <- function() invmatrix ## getter to return solve cache variable
        list(set = set, get = get, ## name each of the list elements so $ can be use to call function 
             setinverse = setinverse,
             getinverse = getinverse)
        }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinverse()  ## get value of the solve cache variable 
        if(!is.null(invmatrix)) { ## if that value if not null then get the value from cash
                message("getting matrix cached data")
                return(invmatrix)
        }
        data <- x$get() ## if that value is null then calculate it and assign it to the cache variable
        invmatrix <- solve(data, ...)
        x$setinverse(invmatrix)
        invmatrix
}

