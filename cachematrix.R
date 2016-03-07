## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Paul Comitz
## Intro to R Programming Assignmnet 2
## this function stores a list of functions 
## those functions are: set,get, setInverse, getInverse
## usage
## a <- makeCacheMatrix(matrix(1:4, nrow=2))
## cacheSolve(a)
## where a is a square matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL
  
  set <- function(y) {
    x <<- y
    inverse  <<- NULL
  }
  
  get <- function() x 
  
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
  
}


##  returns the inverse of the matrix input to the makeCacheMatrix 
##  function if itis is null (has not been previously computed)
##  returns the cahes value if it has been previolusly computed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) { 
      message("getting cached data")
      return (inverse)
    }
    
    data <-x$get()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse
}
