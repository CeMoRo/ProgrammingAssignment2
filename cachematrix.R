## Put comments here that give an overall description of what your
## functions do

## calculating the inverse of a matrix can be a time consuming operation. This functions allow
## us to cache the inverse of a matrix so that it will be there when we need it again and thus, 
## will not be calculated again

## Write a short comment describing this function

## The makeCacheMatrix function creates an R object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function

## CacheSolve takes an argument that is returned by makeCacheMatrix and return the inverse of
## the matrix stored in makeCacheMatrix environment.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}