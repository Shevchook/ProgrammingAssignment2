# CONDITIONS OF THE PROGRAMMING ASSIGNMENT 2
# This fumction creates a special "vector", which is a list containing a
# function to: 1) set the value of the vector; 2) get the value of the vector
# 3) set the value of the mean; 4) get the value of the mean.
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# The following function calculates the mean of the special "vector" created
# with the above function. However, it first checks to see if the mean has
# already been calculated. If so, it gets the mean from the cache and skips the
# computation. Otherwise, it calculates the mean of the data and sets the value
# of the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


# THE SOLUTION
# This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
    
    i <- NULL # Initialize the inverse property
    
    # Setting the matrix:
    set <- function(matrix){
        m <<- matrix
        inv <<- NULL
    }
    
    # Getting the matrix:
    get <- function() {
        m # Return the matrix
    }
    
    # To set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
        
    # Method to get the inverse of the matrix
    getInverse <- function() {
        i # Return the inverse property
    }
    
    # Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function computes the inverse of the special matrix returned by
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve should retrieve the inverse
# from the cache.
cacheSolve <- function(x, ...) {
    
    m <- x$getInverse() # Return a matrix that is the inverse of 'x'
    
    # Just return the inverse if its already set:
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    # Get the matrix from our object:
    data <- x$get()
    
    # Calculate the inverse using matrix multiplication:
    m <- solve(data) %*% data
    
    x$setInverse(m) # Set the inverse to the object
    
    m # Return the matrix
}

# USING THE FUNCTIONS
# Creating an arbitrary matrix
TestMatrix <- makeCacheMatrix(matrix(c(5, 37, 692, 1048), 2, 2))
TestMatrix$get()
cacheSolve(TestMatrix)
cacheSolve(TestMatrix)
TestMatrix$getInverse()
TestMatrix$getInverse()