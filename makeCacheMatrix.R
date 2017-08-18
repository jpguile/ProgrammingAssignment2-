makeCacheMatrix <- function(x = matrix()) {
  reverse <- NULL
  set <- function(y) {
    x <<- y
    reverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) reverse <<- inverse
  getInverse <- function() reverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}