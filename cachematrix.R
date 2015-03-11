## The following functions are used to store a matrix and compute and store 
## the inverse of that matrix


## makeCacheMatrix() stores a matix, x, and its inverse, s

makeCacheMatrix <- function(x = matrix()) { 
    
    s <- NULL
    
    set <- function(y) {                    # Optional: can be called by another function to
                                            # change the stored matrix, x
        x <<- y
        s <<- NULL
    }
    
    get <- function() {x}                   # Returns the object x when called
    
    setinv <- function(inv) {s <<- inv}     # Caches the inverse matrix s
    
    getinv <- function () {s}               # Returns the inverse matrix s when called
    
    list(set = set, get = get,              # Creates and prints a list of the above objects and 
         setinv = setinv, getinv = getinv)  # their assigned functions 
    
}


## cacheSolve() returns a cached object or computes the inverse
## of the matrix stored in list z and caches the result

cacheSolve <- function(z, ...) {            
    
    s <- z$getinv()                         # Retrieve stored value for inverse matrix, s
    
    if(!is.null(s)) {                       # If s is not NULL, return s
        
        message("getting cached data")
        return(s)
    }
    
    data <- z$get()                         # If s is NULL, retrieve matrix from z
    
    s <- solve(data, ...)                   # Compute inverse of matrix
    
    z$setinv(s)                             # Pass inverse of matrix to z for storage
    
    s                                       # Print the inverse matrix
}

