
makeCacheMatrix <- function(x = matrix()) # make a special matrix as an object which is really a list of functions
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #return the matrix which is set by set function
  setinvert <- function(invert) m <<- invert #assign the inverse to m
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert) # return a list of functions
}
cacheSolve <- function(x, ...) ## Return a matrix that is the inverse of 'x'
  {
        m <- x$getinvert()
        if(!is.null(m)) # check for the existing of inverse
        {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)# calculate the inverse matrix and return the value to "m"
        x$setinvert(m)
        m
}
