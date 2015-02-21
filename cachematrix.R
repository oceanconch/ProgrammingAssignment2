## This function creates a special "matrix" object that can cache its inverse.
## 1. "x" is the matrix, 2. get the matrix, 3. set the inverse, and 4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(matrix = matrix()) m <<- matrix
        getinv <- function() m
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## get the stored inverse of the matrix
        m <- x$getinv()
        if(!is.null(m)) { ## non-null value signifies valid cache
                message("getting cached inverse matrix")
                return(m)
        }
        ## otherwise compute the inverse and store in cache
        ## before returning the result
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m ## return the inverse
}





