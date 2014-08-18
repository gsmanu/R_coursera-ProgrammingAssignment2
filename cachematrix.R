## Gurjot Singh

## R function to calculate the inverse of the matrix 
## if not calculated before using cache

## makeCacheMatrix takes the matrix as an input and computes 
## its inverse if not in cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- X$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    print("getting cached data")
    return(inv)
  }
  matrix <- X$get()
  inv <- solve(matrix)
  X$setinverse(inv)
  inv
}
