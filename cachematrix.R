
## Func_Intro:

  #makeCacheMatrix is a R object that will stored a inverse matrix of input x.
  #The inverse matrix is stored so taht we can cache it later to save computation time.

makeCacheMatrix <- function(x = matrix()) {
  
##variable_Intro:
  # x : original matrix, formal argument that will be past when functin initiated
  # inverse_m (variable) : inverse matrix of x
  inverse_m <- NULL
##function_Intro:
  # get : return x (the original matrix)
  get <- function() x
  # get_inverse : return inverse_m (the inverse matrix of x.)
  get_inverse <- function() inverse_m
  # set : it will reset variable x (original matrix), and reset inverse_m to null.
  set <- function(new_matrix = matrix()){
    x <<- new_matrix
    inverse_m <<- NULL
  }
  # set_inverse_matrix: After other function calculate inverse matrix, this function can
  set_inverse_matrix <- function(inverse) inverse_m <<- inverse
  
  #Naming the list element and pass the list to parent environment, so that we can use $operator
  #to call funciton within makeCacheMatrix 
  list(set = set, get_inverse = get_inverse, get = get, set_inverse_matrix = set_inverse_matrix)
}


## Func_Intro:
  #cacheSolve take 1 formal Arguement and object need to be makeCacheMatrix object.
  #it is the function that will calculate inverse matrix and store (using function set_inverse
  #function in makeCacheMatrix to store inverse matrix)

cacheSolve <- function(x, ...) {
    
    
    m <- x$get_inverse()
    #check wheter we had calculated the inverse before. If calculated before, simply return
    #the inverse matrix.
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    # If m is null, it means we haven't calculated the inverse matrix, thus start computing
    matrix <- x$get()
    m <- solve(matrix)
    #store inverser matrix in x(makeCacheMatrix)
    x$set_inverse_matrix(m)
    m
}

