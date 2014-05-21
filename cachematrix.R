## These functions compute and cache the inverse of a matrix. If the
## the inverse was previously computed, then the inverse is recalled
## from cache to save computation time.
##
## Example use in R console:
##  >
##  > A <- makeCacheMatrix(matrix(c(1,2,2,1), 2, 2))
##  > cacheSolve(A)
##          [,1]       [,2]
##  [1,] -0.3333333  0.6666667
##  [2,]  0.6666667 -0.3333333
##  > cacheSolve(A)
##  getting cached data
##          [,1]       [,2]
##  [1,] -0.3333333  0.6666667
##  [2,]  0.6666667 -0.3333333
##  >


## Creates a special "matrix" object that can cache the inverse
## of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    #
    # Args:
    #   x: a matrix to be inverted. The default x is an empty matrix.
    #
    # Returns:
    #   A list of functions which operate on x and m (the inverse of x).
    #   The functions are:
    #   set........assigns a matrix to x and initializes m to NULL. 
    #   get........returns x
    #   setsolve...assigns the inverse to m (the cache)
    #   getsolve...returns m
    
    # Initialize the inverse to NULL
    m <- NULL
    
    set <- function(y) {
        # Assign y to x of the parent (calling) environment
        x <<- y     
        # Assign NULL to m of the parent (calling) environment
        m <<- NULL  
    }
    
    get <- function() x
    # x is a free variable and by lexical scoping
    # is in the parent (calling) environment
    
    setsolve <- function(solve) m <<- solve
    # Assign solve to m of the parent (calling) environment
    
    getsolve <- function() m
    # m is a free variable and by lexical scoping
    # is in the parent (calling) environment is returned
    
    # A list of functions is returned
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
    
}


## Computes and caches the inverse of a matrix. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
    #
    # Args:
    #   x: a list of functions (a special "matrix" object) 
    #      defined in makeCacheMatrix.
    #
    # Returns:
    #   The inverse of the matrix.
    
    # Retrieve the contents of the cache.
    m <- x$getsolve()
    
    # Test if an inverse has been cached.
    if(!is.null(m)) {
        # If an inverse has been cached return it.
        message("getting cached data")
        return(m)
    }
    
    # Inverse has not been cached so get the matrix.
    data <- x$get()
    # Compute the inverse of the matrix
    m <- solve(data, ...)
    # Cache the inverse
    x$setsolve(m)
    # Return the inverse
    m
}