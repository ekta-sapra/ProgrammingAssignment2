## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.

## makeCacheMatrix creates a matrix which is really a list 
## containing functions to setMatrix, getMatrix, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve) m <<- solve
  
  getInverse <- function() m
  
  list(setMatrix = set, getMatrix = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cashSove returns the inverse of 'x' from cache if it exists in cache
## if the inverse of 'x' doesn't exist in cache, 
## it computes the inverse and saves it in cache
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
