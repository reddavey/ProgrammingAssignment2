## These two functions implement a cached inverted matrix. A special matrix
## object (list) is created with makeCacheMatrix(x), where 'x' is a matrix.
## The object contains accessor methods for setting/getting the non-inverted 
## matrix, and for setting/getting the cached inverted matrix.
##
## The second function cacheSolve(x, ...) uses the solve() function to invert
## the matrix in the CacheMatrix object and returns it. If the inverted matrix
## has already been calculated the cached value is returned and not recalculated.

# Defines storage and accessor methods for a cached inverted matrix. 
#
# Args:
#   x: A matrix
#
# Returns:
#   A list with accessor methods for the original matrix and the cached inverted matrix
#   from the original.
#
makeCacheMatrix <- function(x = numeric()) {

    # Symbol to cache the inverted matrix
    invMatrix <- NULL

    # Method to set a new matrix
    set <- function(y) {
        # Reset the cached inverted matrix to NULL if the two matrices 
        # are not identical. This forces the call in cacheSolve to solve(),
        # when the matrix data changes.
        if (! identical(x, y)) {
            x <<- y
            invMatrix <<- NULL
        }
    }

    # Method to return the original matrix
    get <- function() x 

    # Method to set the cached value to an inverted matrix
    setinverse <- function(mtrx) invMatrix <<- mtrx

    # Method to get cached inverted matrix
    getinverse <- function() invMatrix

    # Return the CacheMatrix object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Returns an inverted matrix cached in a 'makeCachedMatrix'. If the cached
# value is NULL then solve() is called to calculate the inverted matrix and it is
# cached in the 'makeCachedMatrix' object.
#   Args:
#     x: A list from 'makeCachedMatrix'
#
#   Returns:
#      An inverted matrix from the matrix stored in 'x'
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getinverse()   # Accesses the object 'x' and gets the cached inverse matrix

    if ( !is.null(invMatrix) ) {  # if the inverse matrix was already cached
        message("getting cached data")  # send a message to the console
        return(invMatrix)   # return the cached inverse matrix value
    }

    # The cached inverse matrix is NULL so now calculate and cache
    # the inverted matrix.

    # Calculate the inverse matrix from the matrix data
    mtrx1 <- x$get()        # Get the original matrix data
    # Invert the matrix
    invMatrix <- solve(mtrx1) 

    x$setinverse(invMatrix) # Cache the inverted matrix 

    invMatrix              # Return the inverted matrix
}
