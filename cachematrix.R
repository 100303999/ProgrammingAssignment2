## This code creates a cached matrix object with its corresponding getter and setter functions

# Function that creates a pseudo vector object with getter and setter functions
makeCacheMatrix <- function(x = matrix()) {
        # Matrix inverse
        m <- NULL
        
        # Sets object attributes (x <- matrix, m <- inverse)
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        
        # Get matrix
        get <- function() x
        
        # Set matrix inverse
        setinverse <- function(inverse) m <<- inverse
        
        # Get matrix inverse
        getinverse <- function() m
        
        # Return list of all object values
        list(set = set, get = set, setinverse = setinverse, getinverse = getinverse)        
}


# Calculate and set matrix x's inverse
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        else {
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
}
}
