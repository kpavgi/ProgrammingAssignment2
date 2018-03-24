#MakeCacheMatrix is a function that can cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  
  make <- function(y) { 
    x <<- y  
    inverse <<- NULL
  }
  #Here are the actual mechanisms that can cache the inverse of the matrix 
  getinv <- function() x 
  makeInverse <- function(invert) inverse <<- invert
  getInverse <- function() inverse
  list(make = make,
       getinv = getinv,
       makeInverse = makeInverse,
       getinv = getinv)
}
#This function checks whether or not the inverse of the matrix exists in the cache, and if it does, returns the inverse. 
#If not, it will solve it here

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if (!is.null(inverse)){return(inverse)
  }
  matrix <- x$getinv()
  inverse <- solve(matrix, ...)
  x$makeInverse(inverse)
  inverse
}

