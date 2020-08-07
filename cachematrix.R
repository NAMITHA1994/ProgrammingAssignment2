## The function will accept a matrix and find its inverse

## This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invr <<- inv
  getinverse <- function() invr
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("Getting cached data :")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data,...)
  x$setinverse(invr)
  invr
}
