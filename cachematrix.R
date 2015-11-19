## Coursera R Programming - Programming Assignment 2: Lexical Scoping
## R function to cache time-consuming computations
## This function will cache the Inverse of a Matrix

## makeCacheMatrix: This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(minv) inv <<- minv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of  the matrix object returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
   ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) 
  {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}
