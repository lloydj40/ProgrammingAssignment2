## Week 3 Peer Review Assignment 2
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: Is a function that computes the inverse of the matrices 
## It contains get, set, getinverse, setinverse
## Assuming Matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve: Is a function that computes the inverse of the matrices 
## returned by makeCacheMatrix above. First checking if the inverse has already
## been calculated.
## If so then the cacheSolve function will collect the inverse from 
## the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("caching inversed matrix")
    return(inv)
  }
  
  ## Computing the inverse of a square matrix can be done with the solve 
  ## function in R. For example, if X is a square invertible matrix, 
  ## then solve(X) returns its inverse.
  
  mtx <- x$get()
  inv <- solve(mtx, ...)
  x$setinverse(inv)
  inv
}
 
