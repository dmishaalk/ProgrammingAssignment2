## Put comments here that give an overall description of what your functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) a <<- inverse
  get_inverse <- function() a
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  a <- x$get_inverse()
  if (!is.null(a)) {
    message("retrieving the inverse from the cache")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$set_inverse(a)
  a
}
