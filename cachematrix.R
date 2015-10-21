## These two functions create a "special" matrix which 
## is actually a list and then use that list to calculate
## the inverse of the matrix

## Create a "special" matrix which is 
## really a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
## set inv to NULL to begin process
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
## assign inv if not already assigned, otherwise return
## original and let user know they already did this once
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


