## This functions calculates the inverse of matrix and then saves it to the cache.

##this first function creates the matrix and stores it in the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function determines whether the inverse of the matrix has been cached already and if not, evaluates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("Getting cached data.")
          return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

