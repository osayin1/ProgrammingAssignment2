## Author: Ozan Sayin 
#  Date:   04/26/2014
#  There are two functions in this script: makeCacheMatrix() and cacheSolve()
#  The idea is that for any given square-invertible matrix we want to cache 
#  the inverse of the matrix after the first query to inverting the matrix 
#  so that we don't compute the inverse over and over again. 
#  makeCacheMatrix() yields a list of functions to get/set the matrix as
#  well as its inverse. cacheSolve() then returns the inverse of the matrix 
## via operating on the function list created by makeCacheMatrix(). 

makeCacheMatrix <- function(x = matrix()) {
## This function returns a special "matrix", namely a list of functions to:  
#  i.  set the matrix to a given matrix,
#  ii. retrieve the matrix 
#  iii.set the inverse of the matrix
#  iv. retrieve the inverse of the matrix  
#  The inverse matrix and the matrix itself can then be accessed via calls to 
## the functions in the list created by this function. 
  
  invx <- NULL 
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setInverse <- function(inv_matrix) invx <<- inv_matrix
  getInverse <- function() invx
  
  # return the list of functions 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
## This function returns the inverse of the matrix on which makeCacheMatrix()
#  was executed. First, it checks whether the inverse has been previously computed
#  and returns it from cache. Otherwise, it computes the inverse and saves it on
#  cache via the special "matrix" created by makeCacheMatrix()
  
  invx <- x$getInverse()
  if(!is.null(invx)) { #already computed..return inverse from cache
    message("getting cached inverse")
    return(invx)
  }
  
  data <- x$get()
  invx <- solve(data) #compute inverse 
  x$setInverse(invx)  #save inverse on cache 
  invx                #return inverse 
}
