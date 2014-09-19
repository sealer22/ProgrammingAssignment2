makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix <- NULL
  ##initialize
  
  set_Original_Matrix <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  ##function to cache the original matrix
  
  get_Original_Matrix <- function() x
  #function to get the original matrix
  
  set_Inverse_Matrix <- function(inverse) inverse_matrix <<- inverse
  #function to cache the inverse matrix
  
  get_Inverse_Matrix <- function() inverse_matrix
  #function to get the inverse matrix
  
  list(set_Original_Matrix = set_Original_Matrix, 
       get_Original_Matrix = get_Original_Matrix, 
       set_Inverse_Matrix = set_Inverse_Matrix, 
       get_Inverse_Matrix = get_Inverse_Matrix)
  #list of all 4 functions
  
}##end makeCachematrix







cacheSolve <- function(x, ...) {

  m <- x$get_Inverse_Matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if the inverse matrix of x already exist, just return the inverse matrix directly
  
  
  
  data <- x$get_Original_Matrix()
  ##get the original matrix
  
  m <- solve(data) %*% data
  ##Get the inverse matrix by the function: solve(x) %*% x
  ##reference page: http://www.endmemo.com/program/R/solve.php
  
  x$set_Inverse_Matrix(m)
  m
  ##cache the inverse matrix and return the result
  
  
}##end cahceSolve