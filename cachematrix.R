## makeCacheMatrix 
## makeCacheMatrix creates objects to store
## an input matrix and its inverse.
## A.Bailey 22 Jan 2015

# Create function for empty matrix x
makeCacheMatrix <- function(x = matrix()) {
        n <- NULL # set placeholder for input matrix n
        m <- NULL # set placeholder for inverse matrix m
        # Define function to set vector x to new vector y
        # and reset m and n to NuLL
        set <- function(y){
                x <<- y 
                n <<- NULL
                m <<- NULL      
        }
        # return the the vector x
        get <- function() x
        # set m to solve
        setsolve <- function(solve) m <<- solve
        setorig <- function(orig) n <<- orig
        # return m to x$getsolve() for calling in cacheSolve 
        getsolve <- function() m
        # return n to x$getorig() for calling in cacheSolve
        getorig <- function () n
        # return a list of the functions
        list(set= set, get=get,
             setsolve=setsolve,
             getsolve=getsolve,
             getorig=getorig,
             setorig=setorig)
}

## cacheSolve
## cacheSolve computes the inverse of the matrix 
## returned by makeCacheMatrix.
## cacheSolve returns the cached matrix if the inverse 
## has already been calculated. 

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        m <- x$getsolve() # set m to value of x$getsolve() from makeCachematrix
        n <- x$getorig() # set n to value of x$getorig() from makeCachematrix
        
        # if n has changed from NULL return cached inverse matrix m
        if (!is.null(n)){
                message("getting cached data")
                return(m)
        }
        # otherwise return vector x to data 
        data <- x$get()
        # solve data and return inverse to m
        m <- solve(data, ...)
        n <- data
        # set x$setsolve to m, and x$setorig to n
        x$setsolve(m)
        x$setorig(n)
        # return m
        m
}
