## Put comments here that give an overall description of what your
## functions do
## The function makeCacheMatrix returns a list of functions that performs
## setMatrix, getMatrix, setInverse, getInverse operations
## The function cacheSolve returns the inverse of the matrix 
## Brief implementation details of each function are described below


## Write a short comment describing this function
## makeCacheMatrix takes a single matrix 'x' as argument and performs :
## - set_matrix assigns x to set_matrix object
## - get_matrix returns the matrix x
## - set_inverse assigns the inverse of x to set_inverse object if exists
##   else assigns NULL
## - get_inverse returns the value of get_inverse object

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set_matrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inverse_t) inverse <<- inverse_t
  get_inverse <- function() inverse
  list(setMatrix = set_matrix, getMatrix = get_matrix, 
       setInverse = set_inverse, getInverse = get_inverse)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of a matrix x. If the inverse already exists 
## in cache i.e. the get_inverse of makeCacheMatrix, the cached inverse is 
## returned or calculates new inverse for matrix stored in the get_matrix object
## of makeCacheMatrix and assigns the value of inverse to the cache i.e 
## set_inverse of makeCacheMatrix using setInverse() function

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
