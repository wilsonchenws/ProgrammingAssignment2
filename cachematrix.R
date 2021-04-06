## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # inverse_m is the variable that store inverse cmatrix we can cache later.
  inverse_m <- NULL
  get <- function() x
  get_inverse <- function() inverse_m
  set <- function(new_matrix = matrix()){
    x <<- new_matrix
    inverse_m <<- NULL
  }
  set_inverse_matrix <- function(inverse) inverse_m <<- inverse
  list(set = set, get_inverse = get_inverse, get = get, set_inverse_matrix = set_inverse_matrix)
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
    m <- solve(matrix)
    x$set_inverse_matrix(m)
    m
}
cm <- matrix(c(4,2,7,6),2,2)
cm2 <- makeCacheMatrix(cm)
cacheSolve(cm2)
cm2$set(matrix(c(3,3.2,3.5,3.6),2,2))
cacheSolve(cm2)