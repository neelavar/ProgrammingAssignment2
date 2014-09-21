## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## This function creates a special "matrix" object that can cache its inverse
##
## Usage: To run these functions
##
## 1. Define a matrix for which inverse has to be calculated
##    example: > xmat <- matrix(c(0.1, 0.1, 0.5, -0.5), nrow=2, ncol=2)
##    This will define a 2 by 2 matrix
##
## 2. Now using this matrix make a Cacheable Matrix
##    example: > cmat <- makeCacheMatrix(xmat)
##
## 3. Now find the inverse of the Cacheable Matrix
##    example: > icmat <- cacheSolve(cmat)
##    observe environment - icmat   num [1:2, 1:2] 5 5 1 -1
##
## 4. Now try computing the inverse again using cacheSolve()
##    example: > icmat <- cacheSOlve(cmat)
##               Retrieving Inverse of the Matrix from the Cache
##             >
##    Observe that the inverse is obtained from cache during second call
##    and not computed again. Try making another cacheable matrix and solve.
##
##

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the inverseM - to hold the inverse of the matrix
  inverseM <- NULL
  
  ## set function - to set the matrix
  ## when the matrix is set (or changed) the inverseM is set to NULL
  set <- function(y){
    x <<- y
    inverseM <<- NULL
  }
  
  ## get function - to retun the matrix
  get <- function() x
  
  ## setInverse function to cache the inverse matrix
  setInverse <- function(invMatrix) inverseM <<- invMatrix
  
  ## getInverse function to return the inverseM which holds cached value of
  ## inverse of the matrix
  getInverse <- function() inverseM
  
  ## makeCacheMatrix function's return value - which is a list of
  ## set & get for matrix and set & get for inverse of the matrix
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache avoiding to compute
## again

cacheSolve <- function(x, ...) {
  ## get the cached value of inverse of the matrix using getInverse()
  inverseM <- x$getInverse()
  
  ## check if inverse matrix value is not NULL
  if(!is.null(inverseM)) {
    ## Since the value is not null return the retrieved inverse matrix from cache
    message("Retrieving Inverse of the Matrix from the Cache")
    return(inverseM) # Function returns from here
  }
  
  ## Now it is clear inverseM is NULL
  ## Hence calculate the inverse matrix using solve() function
  
  ## 1. Get the matrix using get()
  myMatrix <- x$get()
  
  ## 2. Compute the inverse
  inverseM <- solve(myMatrix, ...)
  
  ## 3. Store the inverse matrix into Cache using setInverse()
  x$setInverse(inverseM)
  
  ## Return a matrix that is the inverse of 'x'
  inverseM
}
