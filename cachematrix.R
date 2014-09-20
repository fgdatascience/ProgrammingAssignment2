## Write a short comment describing this function

#create a "cached matrix". It takes as input a matrix (squared, in order to 
#compute its inverse)
makeCacheMatrix <- function(x = numeric() ) {
  
  #inverse matrix initialized with NULL
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #return the matrix
  get <- function() x
  #set the inverse of the matrix
  setInverse <- function(inv) inverse <<- inv
  #get the inverse of the matrix
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the inverse of the matrix from the matrix passed to this function
  inverse <- x$getInverse()
  #if the inverse is not null, it's already computed, so can be returned
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  #otherwise, get the matrix object, and compute it's inverse
  myMatrix <- x$get()
  #check if the matrix is a square matrix
  if(dim(myMatrix)[1] != dim(myMatrix)[2]){
    warning("Input matrix is not a square matrix!")
    return
  }
  #inverse computation
  inverse <- solve(myMatrix, ...)
  #save the inverse in the cache
  x$setinverse(inverse)
  inverse
}