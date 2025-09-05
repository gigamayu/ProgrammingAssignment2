## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## initialize cached inverse as NULL
  set <- function(y) {
    x <<- y ## update matrix
    inv <<- NULL ## reset cached inverse
  }
  get <- function() x ## return the current matrix
  setinverse <- function(inverse)  inv <<- inverse ## store inverse
  getinverse <- function() inv ## return inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data") ## use cached result
    return(inv)
  }
  data <- x$get() ## get matrix
  inv <- solve(data, ...) ## compute inverse
  x$setinverse(inv) ## cache the result
  inv ## return inverse
}
