# Caching the Inverse of a Matrix
#
# Functions:
#       makeCacheMatrix(): 
#           creates a special “matrix” object that can cache its inverse.
#       cacheSolve(): 
#           computes the inverse of the “matrix” returned by makeCacheMatrix(). 
#           If the inverse has already been calculated and the matrix has not changed, 
#           it’ll retrieves the inverse from the cache directly.


# =================
# makeCacheMatrix(): 
# =================
# Creates a special "matrix", which is actually a list containing a function to
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of the inverse matrix
#  4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y     # Use <<- to assign environment from which makeCacheMatrix was called.
                i <<- NULL  # Use <<- to assign 'i' within environment makeCacheMatrix.
        }
        get <- function() x
        setsolve <- function(solv) i <<- solv
        getsolve <- function() i
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# =================
# cacheSolve(): 
# =================
# First chech if a cached inverse matrix has already been set / calculated.
# If the inverse has not been calculated, then calculate and save the result in the cache. 

cacheSolve <- function(x, ...) {
        # First chech if a cached inverse matrix has already been set / calculated.
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # Not in the cache. 
        # Need to calculate the inverse..
        data <- x$get()
        i <- solve(data, ...)
        # .. and store the inverse in the cache.
        x$setsolve(i)
        i
}

