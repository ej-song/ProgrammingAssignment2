## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
## Initialize the inverse property
  inv = NULL
  set = function(y) {
    x <<- y
  inv <<- NULL
  }
  ## Method the get the matrix
  get = function() x
  ## Way to set the inverse of the matrix
  setinv = function(inverse) inv <<- inverse 
  ## Way to get the inverse of the matrix
  getinv = function() inv
  ## Back a list of the methods
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## Compute the inverse of the unique matrix back by "makeCacheMatrix"
## Back to a matrix  “inv”
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## Compute the inverse via matrix multiplication
  data <- x$get()
  m = solve(data)
  ## Set the inverse to the object
  x$setinv(inv)
  ## Coming back the matrix
  return(inv)
}
