## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 

## Your assignment is to write a pair of functions that cache the inverse of a matrix.


## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # prepare inversion variable
  inversion <- NULL
  
  set <- function(y) {
    x <<- y
    inversion <<- NULL
  }
  
  get <- function() x
  setinversion <- function(inv) inversion <<- inv
  getinversion <- function() inversion
  list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}



## The second function calculates the inverse of the "matrix" 
## created with the above function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Read cached inverse matrix
  inv <- x$getinversion()
  
  ## If inverse matrix exists - return cached inversion
  if(!is.null(inv)) {
    message("returning cached data :)")
    return(inv)
  }
  
  ## If inverse matrix not exists - read data, compute inversion and 
  ## cache inversion for future usage
  data <- x$get()
  inv <- solve(data)
  x$setinversion(inv)
  inv
}
