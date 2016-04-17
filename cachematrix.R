## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      # set the value of the matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      #acquire the value of the matrix
      get <- function() x
      #set the value of the inverse matrix
      setinverse <- function(solve) m <<- solve
      # get the value of the inverse matrix
      getinverse <- function() m
      # set return list
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     # check if the inverse is already cached/calculated,
     # if yes, fetch the value from there/cache
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      # if not, get the matrix
      data <- x$get()
      # calculate the inverse
      m <- solve(data, ...)
      x$setinverse(m)
      # Returns the inversed matrix
      return (m)
}
