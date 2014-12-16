## Both functions are analogues of example functions.

## First function gets a matrix as an argument and creates list of 4 functions
## that allow to set and get matrix value and its inverse matrix value.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}

## Second function gets this list as an argument, then checks if inverse matrix value exists
## if yes then it prints existing value, if no it compute new value.
cacheSolve <- function(x) {
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$seti(i)
  i
}
