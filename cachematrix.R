## Put comments here that give an overall description of what your
## functions do

## A square invertible matrix is pass into the makeCacheMatrix function
## To set and get the value of the matrix 
## To set and get the inverse data of the matrix

makeCacheMatrix <- function(x = matrix()) {
 
   m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function verify if the inverse of the matrix has been computed.  
##  If true, then return a message "getting inverse matrix cached data"
##  and the result of the inverse matrix.
##  Else, it compute the inverse of the matrix by calling the makeCacheMatrix
##  function to set and get the inverse of the matrix.  Then called the 
##  setinverse function to cache to matrix.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting inverse matrix cached data")
    return(m)
  }
  
  my_data <- x$get()
  m <- solve(my_data, ...)
  x$setinverse(m)
  m
}
