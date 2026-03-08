## The combination of the two following functions allows for the caching of the
## inverse of a given matrix, instead of calculating it repeatedly.

## This function makeCacheMatrix creates a special "matrix" object, that is 
## a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix 
## 4.  get the value of the inverse of the matrix.

## Overall the special "matrix" object can cache the inverse of the matrix (s).


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list (set = set, 
        get = get,
        setSolve = setsolve,
        getsolve = getsolve)
}

## The function cacheSolve computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`. Firstly, it checks to see if the 
## inverse hast already been calculated and the matrix hasn't been changed since
## then. If that is the case, it retrieves the inverse from the cache and skips 
## the calculation, which is also notified by a message. If that is not the 
## case, it calculates the inverse of the matrix included in the makeCacheMatrix
## list and sets the inverse in the cache via setsolve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}