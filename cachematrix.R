## It's Programming Assigment 2
## Functions are caching the matrix inversion result

## Create special object for cache results of a matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  ## init value of cache variable
  s <- NULL
  
  ## function for setting new matrix
  set <- function(y) {
    ## setting new matrix
    x <<- y
    ## reset value of cache variable
    s <<- NULL
  }
  ## function for getting origonal matrix
  get <- function() x
  
  ## function for storring result
  setSolveMatrix <- function(solveMatrix) s <<- solveMatrix
  
  ## function for getting result
  getSolveMatrix <- function() s
  
  ## list of functions for return
  list(set = set, get = get,
       setSolveMatrix = setSolveMatrix,
       getSolveMatrix = getSolveMatrix)
}


## Function compute inversion of the matrix and put it into cache

cacheSolve <- function(x, ...) {
  ## checking whether there is result in the cache
  s <- x$getSolveMatrix()
  if(!is.null(s)) { ## if exist then return
    message("getting cached data")
    return(s)
  }
  ## getting origonal matrix
  data <- x$get()
  ## computing inversion of matrix
  s <- solve(data)
  ## putting into cache
  x$setSolveMatrix(s)
  ## return s
  s
}
