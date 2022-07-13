## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # set the value of matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get the value of matrix
  get <- function() x
  # set the value of inverse
  setinverse <- function(inverse) i <<- inverse
  # get the value of inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # get the value of inverse of a matrix
  i <- x$getinverse()
  # if it exists (has been calculated), we skip the computation
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # otherwise, we compute the inverse of the matrix using solve()
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
