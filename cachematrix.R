

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # default the value of the cache
  m_cache <- NULL
  # set the value of the matrix
  set <- function(y) {
          x <<- y
          m_cache <<- NULL
  }
  get <- function() x
  setSolution <- function(solution) m_cache <<- solution
  getSolution <- function() m_cache
  list(set = set, get = get,
       setSolution = setSolution,
       getSolution = setSolution)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## See if the cache is available
        cache <- x$getSolution
        
        ## If cache is available, return cache
        if(!is.null(cache)) {
          message("getting cached data")
          return(cache)
        }
        
        ## Otherwise, get the matrix
        matrix_data <- x$get()
        
        ## Return the inverse of the matrix
        inverse <- solve(matrix_data)
        
        ## Set it on the cache
        x$setSolution(inverse)
        
        m_cache
}
