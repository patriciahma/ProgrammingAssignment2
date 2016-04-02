## Put comments here that give an overall description of what your
## functions do

## My function makeCacheMatrix create an object matrix that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) invert <<- solve
  getinvert <- function() invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## cache Solve return a solve matrix (makeCacheMatrix).

cacheSolve <- function(x, ...) {
  invert <- x$getinvert()
  if(!is.null(invert)) {
    message("getting cached matrix")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinvert(invert)
  invert
}

#matrix1 <-matrix(1:4,2,2)
#testa <- makeCacheMatrix(matrix)
#cacheSolve(testa)
