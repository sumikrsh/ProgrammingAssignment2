## Here are 2 functions that accept an input matrix and calculate its inverse. 
## Assume to use only matrices that are inversible. solve() function is used for the inverse calculation. 

###################################################################################
## This function creates a special "matrix" object that can cache its inverse.
## This function accepts an input matrix. The output is actually a list containing
## functions to get and set the matrix and to get and set the inverse matrix. 
###################################################################################

makeCacheMatrix <- function(x){
  
  ## Intializing
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Function to get matrix to be inverted
  get <- function(){
    x
  } 
  
  ## Function to calculate the inverse and store in cache
  setinverse <- function(solve){
    m <<- solve
  } 
  
  ## Function to retrieve inverse from cache
  getinverse <- function(){
    m
  } 
  
  ## Create the special matrix of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}## end of makeCacheMatrix

###################################################################################
## This function computes the inverse of the input matrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
###################################################################################

cacheSolve <- function(x, ...) {
  
  ## Check if the inverse exists in the cache
  m <- x$getinverse() 
  
  ##If yes, get the inverse and return from the function
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  
  ## If inverse is not stored in the cache, calculate the inverse
  data <- x$get() ## get input matrix
  m <- solve(data, ...) ## calculate inverse output
  x$setinverse(m) ## set inverse output
  m ## output inverse
  
} ## end of cacheSolve
