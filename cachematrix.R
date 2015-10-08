## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolveMatrix()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  print(data)
  s <- solve(data)
  x$setSolveMatrix(s)
  s
}
