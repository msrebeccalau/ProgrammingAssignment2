## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse matrix
        setimatrix <- function(solve) m <<- solve
        #get the value of the inverse matrix
        getimatrix <- function() m
        list(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
}


## This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated (and the
#matrix has not changed), then the cachesolve should retrieve the inverse from
#the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getimatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setimatrix(m)
        m
}
