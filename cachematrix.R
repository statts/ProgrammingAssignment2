## Functions associated with ProgrammingAssignment 2 from R programming
## course on Coursera. 
## Example usage:
## mat1 <- rbind(c(1, -1/4), c(-1/4, 1))
## mat2 <- makeCacheMatrix(mat1)
## cacheSolve(mat2)
## Returns:
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## If run a second time, the same result is returned 
## with the added message "getting cached data" showing it is coming from the cache

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL ## reset cache as it is no longer valid
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    ## this means the inverse is already computed and in the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
