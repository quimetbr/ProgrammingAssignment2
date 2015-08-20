## This functions calculates the inverse of a square matrix. 
## To be faster first it's checked if the inverse of the matrix 
## was already created. If yes returns the inverse of the matrix 
## calculated in the past, if not it's calculated and stored in the cache. 

## To create de function I basically take the two examples: makeVector e 
## cachemean and change the function mean of the code by solve. 
## I also rename the variable m for inverse. I also need to redefine the 
## argument of makeCacheMatrix to matrix() to accept matrix and rename 
## the functions with the correct name makeCacheMatrix and cacheSolve

## This function makes the cache of a matrix. Setinverse stores the inverse 
## of the matrix when calculated with solve. getinverse returns the matrix stored. 


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<-solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The function cacheSolve calculates the inverse of a square matrix
## First check if exists the inverse of the matrix in cache, if not calculate the inverse
## and stores the result in cache for the future. 


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
