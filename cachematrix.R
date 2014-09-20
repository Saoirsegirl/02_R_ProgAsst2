## The following program will ingest an matrix of data and "cache" it's inverse to
## make it available for quicker access during repetative calculations within another
## function.

## This function takes in a matrix of contents, creates several several internal
## functions, and using those functions creates cached variable that are ready for
## use by the function cacheSolve or other functions needing quick access to the
## inverse of the same matrix.  Note: the matrix should be square to work consistantly.

makeCacheMatrix <- function(x = matrix()) {
        #makeVector <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #cachemean <- function(x, ...) {
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
                inv
}