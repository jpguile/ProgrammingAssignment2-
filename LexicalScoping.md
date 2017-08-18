# ProgrammingAssignment2-
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

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

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

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

# Testing

> source('C:/Users/jpsantos/AppData/Local/Temp/RtmpOcY4UZ/cacheSolve.R')
> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$set(matrix(c(3, 2, 1, 4), 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    3    1
[2,]    2    4
> my_matrix$getInverse(
+ )
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]  0.4 -0.1
[2,] -0.2  0.3
> cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]  0.4 -0.1
[2,] -0.2  0.3
> my_matrix$getInverse()
     [,1] [,2]
[1,]  0.4 -0.1
[2,] -0.2  0.3
> 
