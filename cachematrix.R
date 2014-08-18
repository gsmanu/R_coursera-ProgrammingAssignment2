## Gurjot Singh

## R function to calculate the inverse of the matrix 
## if not calculated before using cache

## makeCacheMatrix takes the matrix as an input and computes 
## its inverse if not in cache

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(matrix) {
    x <<- matrix
    invm <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invm <<- inverse
  getinverse <- function() invm
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    print("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}
