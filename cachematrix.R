## makeCacheMatrix is a function that stores functions. 
## set() - sets the matrix
## get() - returns the matrix
## setInverse() - sets the inverse of the matrix
## getInverse() - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix(), nrow, ncol) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  
  
  setInverse<- function (solve) inv<<-solve
  getInverse<- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## function checks if the inverse has been calculated before
## if the condition is true, the previously calculated inverse matrix is returned
## otherwise inverse matrix is calculated using R function "solve"

cacheSolve <- function(x) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
