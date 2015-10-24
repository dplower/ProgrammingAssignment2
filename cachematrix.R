## Matrix inversion is usually a costly computation.
## Two new functions will be created to reduce this costly computation
## by helping to cache the inverse of a matrix

## To run this program, please perform the following steps:
## set the working directory to where your cacheMatrix.R file is
## and then load the source, source("cachematrix.R")
## Example Run Below:
## x <- matrix(rnorm(25), nrow = 5)
## x
## [,1]       [,2]        [,3]       [,4]         [,5]
## [1,] -1.3877562  0.8625667  0.52871739 -0.6544809  0.970676033
## [2,] -0.3877907  1.1196761 -0.26387948 -1.5273015 -0.005705022
## [3,] -0.8431350 -1.0348812 -0.07310012 -2.5208573 -0.578842059
## [4,]  0.5455958  1.1054454  1.86400492 -0.8349745 -0.431283256
## [5,] -0.2125786  1.5050134 -1.20309542 -0.6424582 -0.168029391
## m <- makeCacheMatrix(x) 
## cacheSolve(m)
## [,1]       [,2]        [,3]       [,4]         [,5]
## [1,] -0.8958279  2.5200854 -0.75350080 -0.4286233 -1.564725120
## [2,]  0.1540296 -0.2880986 -0.08876107  0.2597346  0.538690775
## [3,]  0.2336677 -0.4799129  0.09804808  0.4027119 -0.005258975
## [4,]  0.3011682 -1.4346817  0.11295091  0.2330483  0.801235219
## [5,] -0.3116249  3.1529932 -0.97563814 -0.9058204 -2.172650784
## cacheSolve(m)
## Getting cached inverse for given matrix
## [,1]       [,2]        [,3]       [,4]         [,5]
## [1,] -0.8958279  2.5200854 -0.75350080 -0.4286233 -1.564725120
## [2,]  0.1540296 -0.2880986 -0.08876107  0.2597346  0.538690775
## [3,]  0.2336677 -0.4799129  0.09804808  0.4027119 -0.005258975
## [4,]  0.3011682 -1.4346817  0.11295091  0.2330483  0.801235219
## [5,] -0.3116249  3.1529932 -0.97563814 -0.9058204 -2.172650784

## This function creates a list of functions to
## set the matrix value
## get the matrix value
## set the matrix inverse value
## get the matrix inverse value
makeCacheMatrix <- function(x = matrix()) {
  theMatrixInverse <- NULL
  
  setTheMatrixValue <- function(y) {
    x <<- y    
    theMatrixInverse <<- NULL # Clear the cache
  }
  
  getTheMatrixValue <- function() x
  
  setTheMatrixInverse <- function(inverse) theMatrixInverse <<- inverse
  getTheMatrixInverse <- function() theMatrixInverse
  
  list(setTheMatrixValue = setTheMatrixValue, 
       getTheMatrixValue = getTheMatrixValue,
       setTheMatrixInverse = setTheMatrixInverse,
       getTheMatrixInverse = getTheMatrixInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  theMatrixInverse <- x$getTheMatrixInverse()
  
  if(!is.null(theMatrixInverse)) { 
    message("Getting cached inverse for given matrix")
    return(theMatrixInverse)
  }
  
  theMatrixData = x$getTheMatrixValue()
  theMatrixInverse = solve(theMatrixData) ## calculating the inverse
  x$setTheMatrixInverse(theMatrixInverse)  ## caching the inverse
  theMatrixInverse
}