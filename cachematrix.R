## Put comments here that give an overall description of what your
## functions do

## take in a matrix to store if previously computed

makeCacheMatrix <- function(x = matrix()){
  
  mat <- NULL
  # set the input matrix "y" to the object "x".
  # place "NULL" for mat as a new matrix is being set and will never 
  # have been cached
  set <- function(y){
    x <<- y
    mat <<- NULL
    
  }
  # this returns the matrix of "x" already set into the function
  get <- function() x
  
  #this allows the user to set an inverse if they choose
  set_invmat <- function(invmat) mat <<- invmat
  
  #outputs the inverse matrix
  get_invmat <- function() mat
  
  list(set = set, get = get, 
       set_invmat = set_invmat, 
       get_invmat = get_invmat)
  
}


## Create inverse matrix or get cached inverse matrix

cacheSolve <- function(x, ...){
  
  # construct the inverse matrix
  mat <- x$get_invmat()
  
  # check if this matrix already exists
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  # return the original matrix before inverse
  data<- x$get()
  
  # if no inverse recorded, compute the inverse
  mat <- solve(data, ...)
  
  # set the inverse of the matrix in cache
  x$set_inv(mat)
  
  # show inverse
  mat
  
}
