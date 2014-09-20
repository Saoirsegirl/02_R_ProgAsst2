## The following is a 2-step program to ingest a matrix of data, "cache" it's inverse
## and make it available for quicker access during repetitive calculations.

## The first function takes in a matrix of contents, creates several internal
## functions, and using those functions creates cached variables that are ready for
## use by the function cacheSolve() or other functions needing quick access to the
## inverse of the matrix defined in the function(arguements). 
## To use: Create an object with this function. Note: the matrix must be square to work.
        ## ex: ma <- makeCacheMatrix(x = your matrix' defintion) 

makeCacheMatrix <- function(x = matrix()) {
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

## Step 2: evaluates if the cache exists, and if not, populates it with the solve()
## created inverse to the matrix defined in the first function.
## Call the cached data using the object of the fist function. i.e. cacheSolve(ma)

cacheSolve <- function(x, ...) {
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

## To test if this program is operating properly, create the inverse of the first matrix
## x = argument, using ma2 <- cacheSolve(ma). print out ma2. Then run makeCacheMatrix(x = ma2),
## and create ma3 <- cacheSolve(ma2).  ma3 should match the original matrix.