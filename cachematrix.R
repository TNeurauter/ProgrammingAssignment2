## Put comments here that give an overall description of what your
## functions do
## Function creates a special Matrix that can cache its inverse
## Write a short comment describing this function
## Take the matrix as an input



  
  makeCacheMatrix <- function(x=matrix())  {
  invMatrix <- NULL
  ## Set the value of the Matrix
  setMatrix<- function(y) { 
    x<<-y
    invMatrix <<-NULL
  }
  
  ## Get the value of the Matrix
  getMatrix <-function()x
  
  ## Set the value of the Inverse
  setInverse <- function(inverse) invMatrix <<-inverse
  
  ## Get the value of the inverse
  getInverse <- function() invMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,  getInverse = getInverse)
  }


## Write a short comment describing this function
## Get the cached value for inverse Matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {  
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  ## if inverse matrix is not Null print message
  ## Getting Cached Data
  
  if(!is.null(invMatrix)) {
    message("Getting Cached Data")
    return(invMatrix)
  }
  ## if value  of the inverse matrix is NULL then 
  
  Matrixdata<- x$getMatrix()
  invMatrix <- solve(Matrixdata, ...)
  x$setInverse(invMatrix)
  return(invMatrix)
  ## Return a matrix that is the inverse of x
  
  
  
  }
