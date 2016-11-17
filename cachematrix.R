## The functions below cache the inverse of a square matrix


## The function makeCacheMatrix takes a square matrix as input 'x' 
## and returns a list containing the following functions:
##   1. SetMat    Sets the matrix
##   2. GetMat    Gets the matrix
##   3. SetInv    Sets the inverse of the matrix
##   4. GetInv    Gets the inverse of the matrix
## The list is used as input to the function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  cacheInp <- NULL
  SetMat <- function(mval) {
    ## Assign the value of 'x' to a new value in an environment different from the current environment (use `<<-`)
    x <<- mval
    ## Then flush the cache
    cacheInp <<- NULL
  }
  GetMat <- function() x
  SetInv <- function(inverse) cacheInp <<- inverse 
  GetInv <- function() cacheInp
  list(SetMat=SetMat, GetMat=GetMat, SetInv=SetInv, GetInv=GetInv)
}


## The function cacheSolve takes the output of the function makeCacheMatrix() as input 'y'
## and returns the inverse of the original matrix input 'x' to makeCacheMatrix()

cacheSolve <- function(y, ...) {
  cacheInp <- y$GetInv()
  
  ## Check if the inverse matrix has already been calculated and if so get it from the cache and skip the computation
  if (!is.null(cacheInp)){
    message("getting cached data")
    return(cacheInp)
  }
  
  ## If not calculate the inverse matrix 
  data <- y$GetMat()
  cacheInp <- solve(data, ...)
  
  # Then set the value of the inverse matrix in the cache by using the SetInv function.
  y$SetInv(cacheInp)
  return(cacheInp)
}
