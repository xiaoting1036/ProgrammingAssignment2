## Matrix inversion
## caching the inverse of a matrix rather than compute it repeatedly 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setInverse <- function(inverse) m <<- inverse
    
    # Get the inverse
    getInverse <- function() m
    
    # Return a list of functions
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
##cacheSolve Function
cacheSolve <- function(x, ...) {
    # Check if inverse is already cached
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached inverse data")
        return(m)
    }
    # If not, compute the inverse
    mat <- x$get()
    m <- solve(mat, ...)
    x$setInverse(m)
    
    m #Return the inverse
}
# Create a square invertible matrix
mat <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
# Create the special matrix object
special_mat <- makeCacheMatrix(mat)
# Compute inverse (first time, computes and caches)
cacheSolve(special_mat)
# Retrieve cached inverse (second time, retrieves from cache)
cacheSolve(special_mat)
