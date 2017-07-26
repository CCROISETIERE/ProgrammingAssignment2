## Put comments here that give an overall description of what your
## functions do


## This function set and get a cache varaible in the parent environment 
## and create a list of function that can be called by cacheSolve using $

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


## This function check if the solve for inverted matrix was already calculated and store in the cache
## If it wasn't store it will calculated it and store the value to save computing time 

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

