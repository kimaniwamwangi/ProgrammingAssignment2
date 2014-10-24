setwd("~/Documents/rprogramming/ProgrammingAssignment2/")
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set default the inverse for the 
  k <- NULL
  set <- function(y) {
    #cache the values
    x <<- y
    k <<- NULL
  }
  #get functions
  get <- function() x
  #cache the inverse
  setinv <- function(inv) k <<- inv
  #return the inverse
  getinv <- function() k
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  #check if matrix fits the bill for a rectangular matrix
  if (nrow(x$get()) != ncol(x$get())) {
    message("Please use a rectangular matrix")
    return()
  }
  ## Return a matrix that is the inverse of x
  i <- x$getinv()
  if(!is.null(i)) {
    message("Accessing Cache for Matrix")
    return(i)
  }
  data <- x$get()
  #solve the matrix
  i <- solve(data, ...)
  #set the inverse
  x$setinv(i)
  #return the inverse
  i
}
