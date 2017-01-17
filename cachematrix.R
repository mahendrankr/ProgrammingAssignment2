> ## Our aim in this experiment is to write a pair of functions, namely, 
> ## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
> ## makeCacheMatrix is a function which creates a special "matrix" object that can
> ## cache its inverse for the input (which is an invertible square matrix)
> makeCacheMatrix <- function(x = matrix()) {
+   inv <- NULL
+   set <- function(y) {
+     x <<- y
+     inv <<- NULL
+   }
+   get <- function() x
+   setinv <- function(inverse) inv <<- inverse
+   getinv <- function() inv
+   list(set = set, get = get, setinv = setinv, getinv = getinv)
+ }
> ## cacheSolve is a function which computes the inverse of the special "matrix"
> ## returned by makeCacheMatrix above. If the inverse has already been calculated
> ## (and the matrix has not changed), then the cachesolve should retrieve the 
> ## inverse from the cache
> cacheSolve <- function(x, ...) {
+   ## Return a matrix that is the inverse of 'x'
+   inv <- x$getinv()
+   if(!is.null(inv)) {
+     message("getting cached result")
+     return(inv)
+   }
+   data <- x$get()
+   inv <- solve(data, ...)
+   x$setinv(inv)
+   inv
+ }
> ## ---------------Checking the program------------------------
> ## m <- matrix(rnorm(16),4,4)
> ## m1 <- makeCacheMatrix(m)
> ## cacheSolve(m1)
> m <- matrix(rnorm(16),4,4)
> m1 <- makeCacheMatrix(m)
> cacheSolve(m1)
           [,1]       [,2]       [,3]       [,4]
[1,]  4.1409003 -2.1328335  4.0156967 -1.4479288
[2,] -0.9047731  0.6479864 -1.0008469 -0.1096492
[3,] -1.6666513  0.7076352 -0.9324443  0.5566875
[4,] -3.6137942  2.5761110 -2.9841619  1.2945955
