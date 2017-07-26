

## This function set and get a cache variable in the parent environment 
## It will create a list of function that can be called by cacheSolve using $

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL                            ## reset cache variable when the matrix is modified
        set <- function(y) {                         ## setter to pass y to x in the parent environment
                x <-- y
                invmatrix <<- NULL                   ## reset cache variable when matrix is modified
        }
        get <- function() x                          ## return x 
        setinverse <- function(inversematrix) invmatrix <<- inversematrix    ## set in the parent environment 
        getinverse <- function() invmatrix                                   ## getter to return solve cache variable
        list(set = set, get = get,                                           ## name each of the list elements so $ can be use to call the functions 
             setinverse = setinverse,
             getinverse = getinverse)
        }


## This function checks if the inverted matrix was already calculated and stores in the cache
## If it wasn't store it will calculated it and store the value instead

cacheSolve <- function(x, ...) {                     ## return a matrix that is the inverse of 'x' (makeCacheMatrix)
                                       
        invmatrix <- x$getinverse()                  ## get value of the solved cache variable 
        if(!is.null(invmatrix)) {                    ## if that value if not null then get the value from cash
                message("getting matrix cached data")
                return(invmatrix)
        }
        data <- x$get()                              ## if that value is null then calculate it and assign it to the cache variable
        invmatrix <- solve(data, ...)
        x$setinverse(invmatrix)
        invmatrix
}

