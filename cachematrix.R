## Put comments here that give an overall description of what your
## functions do

## Create a "special" matrix which is 
## really a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inver) inv <<- inver
  getInv <- function() inv
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
  }

 ## Function to calculate the inverse of the "special" 
 ##  matrix created above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getInv()
  if(!is.null(inv)){
    message("getting cached data, no need to recompute")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}


