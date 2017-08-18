cacheSolve <- function(x, ...) {
  reverse <- x$getInverse()
  if (!is.null(reverse)) {
    message("getting cached data")
    return(reverse)
  }
  mat <- x$get()
  reverse <- solve(mat, ...)
  x$setInverse(reverse)
  reverse
}
