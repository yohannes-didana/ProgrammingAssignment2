## caching the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                             # cached value of inverse, if exists
  set <- function(mat) {                      #set function: assign mat to x and NULL to inverse
    x <<- mat                                 # save matrix to this functions environment
    inverse <<- NULL                          # invalidate cached inverse
  }
  
  
  get <- function() x                           # return matrix
  setinverse <- function(inv) inverse <<- inv   # cache the inverse
  getinverse <- function() inverse
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()     # Return a matrix that is the inverse of 'x'
  if(!is.null(inverse)) {
    
    # if cached inverse exists, return the following message
    
    message("returning cached inverse")
    return(inverse)
  }
  data <- x$get()               # extract the underlying data
  inverse <- solve(data, ...)   # calculate its inverse
  x$setinverse(inverse)         # cache the inverse for re-use
  inverse                       # return inverse
}