## The following pair of functions cache the inverse of a matrix.
## "makeCacheMatrix" function creates a special "matrix" object that can cache 
## its inverse.
## "cacheSolve" function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
  ### Return a list of setter and getter functions 
  inv_matrix <- NULL
  set_matrix <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get_matrix <- function() {
    x
  }
  set_inverted_matrix <- function(inverted) {
    inv_matrix <<- inverted
  }
  get_inverted_matrix <- function() {
    inv_matrix
  }
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverted_matrix = set_inverted_matrix,
       get_inverted_matrix = get_inverted_matrix)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverted_matrix()
  if (!is.null(inv)) {
    message("getting cached inverted matrix")
    return(inv)
  }
  message("really inverting matrix")
  matrix <- x$get_matrix()
  inv <- solve(matrix)
  x$set_inverted_matrix(inv)
  inv
}
