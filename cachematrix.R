# The two functions below are used to compute and store the inverse of a matrix 
# and retrieve this result from cache in the future, if it has already been
# computed.


# The makeCacheMatrix function creates an object that stores the input matrix
# 'x', the inverse 'm', and the object methods to access and modify 'x' and 'm'.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                       # m is our inverse and will be reset to NULL
                                    # every time makeCacheMatrix is called.
    
    set <- function(y) {            # This object method can be called to
        x <<- y                     # update the matrix using set()
        m <<- NULL
    }
    get <- function() x                        # Returns the matrix when called.
    setInv <- function(solve) m <<- solve      # Caches the inverse when called.
    getInv <- function() m             # Returns the cached inverse when called.
    
    list(set = set, get = get, # This list is accessed each time makeCacheMatrix
         setInv = setInv,      # is called. It lists object methods so the
         getInv = getInv)      # calling function knows how to access them.
}


# The cacheSolve function checks if the inverse 'm' has already been computed
# and returns a message and the result if it has. Otherwise, it accesses matrix
# 'x' and computes, stores, and returns the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getInv()           # Assigns the inverse from the input object to 'm'
    
    if(!is.null(m)) {                      # If inverse is cached ('m' not null)
        message("getting cached data")     # send this message to the console
        return(m)                          # and return the inverse.
    }
    data <- x$get()                    # Assigns the original matrix 'x' to data
    m <- solve(data, ...)              # Computes the inverse and assigns to 'm'
    x$setInv(m)                        # Stores the computed inverse to cache.
    m                                  # Returns the inverse.
}
