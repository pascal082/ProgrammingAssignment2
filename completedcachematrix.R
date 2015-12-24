
## These 2 functions introduce a special matrix object which caches its inverse.


#' Create a special "matrix" object that can cache its inverse.
#' 
#'     
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Define function to set the value of the matrix. It also clears the old
    # inverse from the cache
    set <- function(y) {
        x <<- y    # Set the value
        m <<- NULL # Clear the cache
    }
    # Define function to get the value of the matrix
    get <- function() x
       setInverse <- function(inverse) m <<- inverse
       getInverse <- function() m
    
    # Return a list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#' Return inverse of matrix x
#' 
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve retrieves the 
#' inverse from the cache.
#' 

cacheSolve <- function(x) {
 # fetches the cache value for the inverse

    m <- x$getInverse()
# If the cache is not empty, then return it

    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    # if cache is empty. we need to calculate it, cache it, before returning it.
    data <- x$get()  
    m <- solve(data) 
    x$setInverse(m)  
    m                
}


