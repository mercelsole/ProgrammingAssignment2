# Assignment 2: Caching the Inverse of a Matrix:
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The assignment is to write a pair of functions that cache the inverse of a
# matrix.

# makeCacheMatrix: function that creates a special "matrix" object that can 
# cache its inverse.
makeCacheMatrix <-function(x=matrix()) {
   Inverse<-NULL
   set <- function(y) {
      x <<- y
      Inverse <<- NULL
   }
   get <- function() x
   setInverse <- function(Inv) Inverse <<- Inv
   getInverse <- function() Inverse
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve: function that computes the inverse of the special "matrix" 
# returned by makeCacheMatrix function. 
# If the inverse has already been calculated (and the matrix has not changed), 
# it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
   Inverse <- x$getInverse()
   if(!is.null(Inverse)) {
      message("getting cached data")
      return(Inverse)
   }
   data <- x$get()
   Inverse <- solve(data, ...)
   x$setInverse(Inverse)
   Inverse
}

#Here's an example for testing:
#> m<-matrix(10:13, 2, 2)
#     [,1] [,2]
#[1,]   10   12
#[2,]   11   13
#> m1 <- makeCacheMatrix(m)
#> m1$get()
#> class(m1$get())
#> m2<-cacheSolve(m1)
#> class(m2)
#> m2
#     [,1] [,2]
#[1,] -6.5    6
#[2,]  5.5   -5
#> m2<-cacheSolve(m1)
#getting cached data
#> m1$get() %*% m2
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
