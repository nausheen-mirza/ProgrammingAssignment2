# The function caches the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      invr <- NULL
      set <- function(y) {
            x <<- y
            invr <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) invr <<- inverse
      getinverse <- function() invr
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve returs the inverse of the matrix. Firstly it checks if
# the inverse has already been computed. If yes, it gets the result from the cache 
# and skips the computation. Else, it computes the inverse, sets the value in the 
# cache via setinverse function.

# It assumes the matrix to be inversive

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}