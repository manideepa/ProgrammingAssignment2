makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Initialize the local m to NULL
  set <- function(y) { ## Create set function to store the matrix passed in the call as x and NULL as m, both in cache.
    x <<- y ## Put the initial matrix from the command line into cache as x
    m <<- NULL ## Initialize m to NULL
  }
  get <- function() x ## Create function to get/return the matrix passed in the command line
  setmatrix <- function(solve) m <<- solve ## Create function to set the value of m in cache to the value of solve
  getmatrix <- function() m ## Create function to retrieve value of m from cache and return m to the caller so we can check it for NULL
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



cacheSolve <- function(x = matrix(), ...) {
  m <- x$getmatrix() ## Get the value for m in the cache environment and put it in a local m.
  if(!is.null(m)) { ## Check to see if m is NULL
    message("getting cached data") ## If m is not NULL, return the value of m with a message
    return(m)
  } ## If we get to this line, m was NULL
  matrix <- x$get() ## Call the nested function x$get in makeCacheMatrix to obtain the UNinverted matrix with which to start, and assign it to matrix.
  m <- solve(matrix, ...) ## Use solve() to invert the startingmatrix. Assign the result to m.
  x$setmatrix(m) ## Call nested function x$setmatrix() in makeCacheMatrix to set m in the cache environment to the local non-NULL inverted result in m
  m ## Evaluate m so as to return it to caller/console if m is non NULL.
}