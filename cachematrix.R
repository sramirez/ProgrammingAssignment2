## This is an improvement in efficiency for Matrix inversion in R.
## This has the proper methods to cache ## and retrive the inverse of a matrix  (in case of the inverse is already computed). 
## We assume that the matrix is invertible and square.

## This function creates a cacheable matrix object that computes the inverse of a matrix, 
## storing this matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of a matrix returned by makeCacheMatrix.
## If the inverse is cached (and the matrix has not changed), then it returns this 
## If not, it computes the inverse from the input matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  data <- x$get()
  if(!is.null(inv) && isTRUE(all.equal(inv, data))) {
    message("getting inverse matrix")
    return(inv)
  }
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
