## Calculate the inverse of a matrix.  Return cached matrix if 
## it exists.  Otherwise calculate inverse matrix.

## Make a cachable matrix object
## Exposes:
##    setMatrix() - Set the matrix to have inverse calculated
##    getMatrix() - Get the matrix to have inverse calculated
##    setInverseMatrix() - Set the inverse matrix into cache
##    getInverseMatrix() - Get inverse matrix from cache
makeCacheMatrix <- function(x = matrix()) {
  cachedInverseMatrix <- NULL
  setMatrix <- function(inMatrix){
    x <<- inMatrix
    cachedMatrix <<- NULL
  }
  
  getMatrix <- function(){
    x
  }
  
  setInverseMatrix <- function(inverseMatrix){
    cachedInverseMatrix <<- inverseMatrix
  }
  
  getInverseMatrix <- function(){
    cachedInverseMatrix
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
  
}


## Using a matrix created with makeCacheMatrix calculate inverse.
## If cached matrix exists return cached matrix otherwise calculate inverse
## and cache result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invMatrix <- x$getInverseMatrix()
  if(!is.null(invMatrix)){
    message("Getting Cached Data")
    return(invMatrix)
  }
  matrixToInvert <- x$getMatrix()
  inverseMatrix <- solve(matrixToInvert)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
