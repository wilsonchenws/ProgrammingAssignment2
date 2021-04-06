## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # inverse_m is the variable that store inverse cmatrix we can cache later.
  inverse_m <- NULL
  get_inverse <- function() inverse_m
  get <- function() x
  set_inverse_matrix <- function(inverse) inverse_m <<- inverse
  #list(get_inverse = get_inverse, get = get, set_inverse_matrix = set_inverse_matrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(data)
    x$set_inverse_matrix(m)
    m
}
