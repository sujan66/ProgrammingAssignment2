## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set_matrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inverse_t) inverse <<- inverse_t
  get_inverse <- function() inverse
  list(setMatrix = set_matrix, getMatrix = get_matrix, setInverse = set_inverse, getInverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Returning cached data")
    return(inverse)
  }
  matrix_data <- x$getMatrix()
  inverse <- solve(matrix_data)
  x$setInverse(inverse)
  inverse
}
