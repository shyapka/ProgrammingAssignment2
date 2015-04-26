## With the help of these functions we can get inverse of a matrix 
## cached for the first time and fetch it for subsequent fetch


## First function creates a vector that has four functions
makeCacheMatrix <- function(x=matrix()) {
  
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInv <- function(inv) I <<- inv
  getInv <- function() I
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Second function checks if Inverse is already in the cache. If not, it would
## Calculate the inverse and cache it for future use

cacheSolve <- function(x,...) {
  
  I <- x$getInv()
 if (!is.null(I)) {
   message("getting cached data")
   return(I)
 }
 data <- x$get()
 I <- solve(data, ...)
 x$setInv(I)
 I   
  
}