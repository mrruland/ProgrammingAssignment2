## This function creats an environment for the cached Matrix 
## the result of execution is that a vector with 4 functions is defined and the 
## cached area is initialize - there are basically 2 variables in the cached area:
##   - cachedMatrix - contains the matrix used during creation of the cached environment
##   - cachedInverse - contains NULL if the cacheSolve function has not been executed or t
##     inverse of the matrix in cachedMatrix
##
## the 4 functions created and passed back in the list are:
##   - setMatrix - initialzes the cache area when called and returns the list of functions 
##     associated with the environment created for this matrix 
##   - getMatrix - returns the matrix used to establish the environment
##   - setInverse - sets the Inverse of the matrix used to establish the environment (it is 
##     NULL until the cacheSolve function is called) - this function can also be called 
##     from the command line to manually populate the cachedInverse variable 
##   - getInverse - returns the cached value of the Inverse matrix

makeCacheMatrix <- function(theMatrix = matrix()) {
  cachedInverse <- NULL
  ##  setMatrix intializes the environment
  setMatrix <- function(theMatrix) {
    cachedMatrix <<- theMatrix
    cachedInverse <<- NULL
  }
  getMatrix <- function() theMatrix  ## getMatrix returns the matrix used to create the environment
  setInverse <- function(inverse) cachedInverse <<- inverse ## sets the value of the Inverse matrix 
  getInverse <- function() cachedInverse  ## returns the value of the Inverse matrix
##  this list is returned as a result of executing the function and associates the 4 functions 
##  with the environment - they are called by specifying the environment set during initialization 
##  concatonated with the function name (e.g. lm$getMatrix where lm is the variable used in
##  the initialization call to this function)
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is called return a matrix that is the inverset of the matrix used to establish
## the cached environmnet, if the value of the inverse in the cached environmnet is NULL, 
## cacheSolve will calculate the inverse and call the setInverse function in makeCacheMatrix 
## to set the value of the inverse matrix in the cached environment.
## Note that the makeCacheMatrix function creates the environment that the cached values are stored in 
## so makeCacheMatrix needs to be called prior to calling cacheSolve 

cacheSolve <- function(cachedEnv, ...) {
        ## Return a matrix that is the inverse of 'x'
  theInverse <- cachedEnv$getInverse()  ## get the currently cached inverse value
  if(!is.null(theInverse)) {
    message("getting cached data")  ## if the cached inverse was not null let the user know
    return(theInverse)                       ## we are returning a stored value rather than calculating it
  }
  theMatrix <- cachedEnv$getMatrix()   ## get the original matrix to solve for 
  theInverse <- solve(theMatrix, ...)  ## calculate the inverse
  cachedEnv$setInverse(theInverse)     ## store it in the cached area
  theInverse	                         ## return the calculated inverse value to the caller
}
