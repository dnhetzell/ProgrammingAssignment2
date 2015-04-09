## Pair of functions which return a special matrix with attached Cache informaton
##  and also caches inverse of the matrix, assumes matrix passed through is square
## and inversable

## Creates Special Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y ##caches matrix as a variable for recall
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(x){ m <<- x} ##caches matrix when called meant to cache matrix which function has taken place
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## creates and stores inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' assumes x is the special matrix created using
        ## makeCacheMatrix function
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
}
