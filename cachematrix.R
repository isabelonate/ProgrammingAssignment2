## In this excercise I create 2 functions for the purpose of 
## caching the inverse of a matrix.

## The first function called makeCacheMatrix defines 4 different 
## functions that perform the following functions: 
## set the value of a matrix, get this value, set the value 
## of its inverse, and geting it. These are the set, get, setinv 
## and getinv functions respectively.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat<-NULL
  set <- function(y) {
      x <<- y
      inv_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_mat <<- inverse
  getinverse <- function() inv_mat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function called cacheSolve does the followig. 
## Checks if the inverse of a matrix has already been defined. 
## In this case it reports it. If it has not been defined previously, 
## it calculates it and stores it.

cacheSolve <- function(x, ...) {
  inv_mat <- x$getinverse()
  if (!is.null(inv_mat)) {
      message ("getting cached data")
      return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data)
  x$setinverse(inv_mat)
  inv_mat
}
