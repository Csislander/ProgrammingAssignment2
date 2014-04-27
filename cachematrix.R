## These two functions (makeCahceMatrix, cacheSolve) are used together to
## create a special matrix and compute its inverse, but if its inverse has
## been previously calculated, it will simply look up the inverse value
## rather than redoing the timely inverse procedure.

## Assuming a matrix is invertible, makeCahceMatrix takes in a matrix and
## creates a special matrix object that can cache the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
      x<<-y
      m<<-NULL
  }
  get <- function() x
  setInverse <-function(inverse) m <<- inverse
  getInverse <-function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve checks to see if the inverse of a matrix has already
## been calculated and retrieves it if it hasn't, or inverts it if it hasn't

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
