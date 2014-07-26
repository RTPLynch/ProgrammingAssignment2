## RTPLynch

## makceCacheMatrixList is a function that takes a matrix as argument
## and returns a list object which contains four subobjects :
## get (which returns the input matrix argument)
## set which allows one to store a new matrix
## setInv which allows one to store an Inverse matrix
## and getInv which retrieves the value of the Inverse of the input argument
## but only if done after first calling cacheInv , otherwise it returns null

makeCacheMatrixList <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setInv <- function(Inv) I <<- Inv
      getInv <- function() I
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## cacheInv is a function that takes a matrix as argument
## and returns its inverse
## Where the inverse has already been computed it returns the value stored in cache 
## Where the inverse has not yet been computed , it calculates the inverse
## then calls the makeCacheMatrixList function's output argument setInv to store
## the value of the Inverse

cacheInv <- function(x, ...) {
      I <- x$getInv()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data, ...)
      x$setInv(I)
      I
}
