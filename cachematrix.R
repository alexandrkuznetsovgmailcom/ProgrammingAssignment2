## It's Programming Assigment 2
## Functions are caching the matrix inversion result

## Create special object for cache results of a matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolveMatrix <- function(solveMatrix) s <<- solveMatrix
  getSolveMatrix <- function() s
  list(set = set, get = get,
       setSolveMatrix = setSolveMatrix,
       getSolveMatrix = getSolveMatrix)
}


## Function compute inversion of the matrix and put it into cache

cacheSolve <- function(x, ...) {
  ## checking whether there is result in the cache
  x$get()
  x$getSolveMatrix()
  s <- x$getSolveMatrix()
  if(!is.null(s)) { ## if exist then return
    message("getting cached data")
    return(s)
  }
  ## getting orogonal matrix
  data <- x$get()
  ## computing inversion of matrix
  s <- solve(data)
  ## putting into cache
  x$setSolveMatrix(s)
  ## return s
  s
}
