## Caching the Inverse of a Matrix

## a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    Inverse<<- NULL
}
  get <- function() x
  setMatrix <- function(MatrixInverse) Inverse <<- MatrixInverse
  getMatrix <- function() Inverse
  list(set = set, get = get,setMatrix = setMatrix,getMatrix = getMatrix)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  Inverse <- x$getMatrix()
  if(!is.null(Inverse)) {
    message("cached inverse from matrix data")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setMatrix(Inverse)
        ## Return a matrix that is the inverse of 'x'
  Inverse
}
