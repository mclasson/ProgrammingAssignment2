## Functions for caching the inverse of a matrix


## makeCacheMatrix is a factory method for creating cachable matrices
## It wraps both the original matrix and its inverse for easy access

makeCacheMatrix <- function(x = matrix()) {
    ## The local container is initialized to NULL
    cachedInverseMatrix <- NULL 

    ## The setter resets the matrix wrapper by setting the original matrix, x, to the new value y
    ## and re-initializing the inverse to NULL
    set <- function(y) {
        x <<- y
        cachedInverseMatrix <<- NULL
    }
    
    ## the getter simply returns the original matrix
    get <- function() x
    
    ## setInverse takes a inverted matrix as a param and updates the local container
    setInverse <- function(inverse) cachedInverseMatrix <<- inverse
    
    ## getInverse simply returns the stored inverted matrix
    getInverse <- function() cachedInverseMatrix
    
    ## The factory method return an object with the four methods. These will be able to access
    ## the internal, local, values (e.g. cachedInverseMatrix) through closure
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve method is a helper method for utilizing the cachable matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Try to get the already cached inverse
    inverse <- x$getInverse()
    
    ## If the result is not NULL, the cache is already initilized
    if(!is.null(inverse)){
        ## Simply return the inverse
        return(inverse)
    }
    
    ## If the result was NULL, the cache has to be initialized
    
    ## First get the original matrix
    original <- x$get()
    
    ## Invert it
    inverse <- solve(original)
    
    ## Use the setter to store the inverse in the cache
    x$setInverse(inverse)
    
    ## Return the now cached inverse
    inverse
    
}
