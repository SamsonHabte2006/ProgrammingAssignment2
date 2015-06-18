## The first function creates a special 'matrix', which is really a list
## containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix
## get the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)


}

## A function that returns an inverse of a matrix
## It returns from cache if it exists other wise it computes.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
  
}
